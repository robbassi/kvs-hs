module RBTree where

import Types (Key (..), Value (..))
import Data.IORef
import Data.Word
import Data.Foldable (for_)
import Control.Monad (when, unless)
import qualified Data.ByteString as BS

data Color = Red | Black deriving (Eq, Show)

data Links = Links { parent :: IORef RBTreeNode
                   , left :: IORef RBTreeNode
                   , right :: IORef RBTreeNode }

data RBTreeNode = Node { nodeKey :: Key
                       , nodeValue :: IORef Value
                       , nodeColor :: IORef Color
                       , nodeLinks :: Links }
                | Empty

newtype RBTree = RBTree (IORef (Maybe RBTreeNode))

instance Eq RBTreeNode where
  Empty == _ = False
  _ == Empty = False
  Node key _ _ _ == Node key' _ _ _ = key == key'

grandparent :: RBTreeNode -> IO (Maybe RBTreeNode)
grandparent Empty = pure Nothing
grandparent (Node _ _ _ (Links parentRef _ _)) = do
  parent <- readIORef parentRef
  case parent of
    Empty -> pure Nothing
    Node _ _ _ (Links grandparentRef _ _) -> do
      grandparent <- readIORef grandparentRef
      pure $ case grandparent of
        Empty -> Nothing
        node -> Just node

uncle :: RBTreeNode -> IO (Maybe RBTreeNode)
uncle Empty = pure Nothing
uncle node@(Node _ _ _ (Links parentRef _ _)) = do
  parent <- readIORef parentRef
  mGrandparent <- grandparent node
  case (parent, mGrandparent) of
    (Empty, _) -> pure Nothing
    (parentNode, Nothing) -> pure Nothing
    (parentNode, Just (Node _ _ _ (Links _ leftRef rightRef))) -> do
      left <- readIORef leftRef
      right <- readIORef rightRef
      pure $ Just $ if left == parentNode then right else left

empty :: IO RBTree
empty = RBTree <$> newIORef Nothing

toList :: RBTree -> IO [(Key, Value)]
toList (RBTree rootRef) = do
  mRoot <- readIORef rootRef
  case mRoot of
    Nothing -> pure []
    Just Empty -> pure []
    Just node -> toList' node

toList' :: RBTreeNode -> IO [(Key, Value)]
toList' Empty = pure []
toList' (Node k valueRef _ (Links _ leftRef rightRef)) = do
  left <- readIORef leftRef
  right <- readIORef rightRef
  value <- readIORef valueRef
  tail <- toList' left
  let tail' = tail ++ [(k, value)]
  rightTail <- toList' right
  pure $ tail' ++ rightTail

search :: RBTree -> Key -> IO (Maybe Value)
search (RBTree rootRef) key = do
  mRoot <- readIORef rootRef
  case mRoot of
    Nothing -> pure Nothing
    Just root -> search' root key

search' :: RBTreeNode -> Key -> IO (Maybe Value)
search' Empty _ = pure Nothing
search' (Node key valueRef _ (Links _ leftRef rightRef)) searchKey = do
  case compare searchKey key of
    LT -> do
      left <- readIORef leftRef
      search' left searchKey
    EQ -> do
      value <- readIORef valueRef
      pure $ Just value
    GT -> do
      right <- readIORef rightRef
      search' right searchKey

mkNode :: Key -> Value -> Color -> RBTreeNode -> RBTreeNode -> RBTreeNode -> IO RBTreeNode
mkNode nodeKey value color parentNode leftNode rightNode = do
  nodeValue <- newIORef value
  nodeColor <- newIORef color
  nodeLinks <- newNodeLinks
  pure $ Node {..}
  where newNodeLinks = do
          parent <- newIORef parentNode
          left <- newIORef leftNode
          right <- newIORef rightNode
          pure $ Links {..}

findRoot :: RBTreeNode -> IO RBTreeNode
findRoot Empty = pure Empty
findRoot node@(Node _ _ _ (Links parentRef _ _)) = do
  mParent <- readIORef parentRef
  case mParent of
    Empty -> pure node
    parent -> findRoot parent

insert :: RBTree -> Key -> Value -> IO ()
insert (RBTree rootRef) k v = do
  mRoot <- readIORef rootRef
  case mRoot of
    Nothing -> writeIORef rootRef =<< pure . Just =<< mkNode k v Black Empty Empty Empty
    Just node -> do
      mNewNode <- insert' node k v
      case mNewNode of
        Nothing -> pure ()
        Just newNode -> do
          newRoot <- findRoot newNode
          writeIORef rootRef $ Just newRoot

insert' :: RBTreeNode -> Key -> Value -> IO (Maybe RBTreeNode)
insert' Empty k v = error "Invariant violated: insert' called with Empty root"
insert' root@(Node key valueRef _ (Links _ leftRef rightRef)) newKey newValue = do
  case compare newKey key of
    EQ -> do
      writeIORef valueRef newValue
      pure Nothing
    LT -> do
      left <- readIORef leftRef
      case left of
        Empty -> do
          newNode <- mkNode newKey newValue Red root Empty Empty
          writeIORef leftRef newNode
          insertRepair newNode
          pure $ Just newNode
        node -> insert' node newKey newValue
    GT -> do
      right <- readIORef rightRef
      case right of
        Empty -> do
          newNode <- mkNode newKey newValue Red root Empty Empty
          writeIORef rightRef newNode
          insertRepair newNode
          pure $ Just newNode
        node -> insert' node newKey newValue

insertRepair :: RBTreeNode -> IO ()
insertRepair Empty = pure ()
insertRepair node@(Node _ _ colorRef (Links parentRef leftRef rightRef)) = do
  parent <- readIORef parentRef
  mUncle <- uncle node
  parentColor <- case parent of
    Empty -> pure Nothing
    Node _ _ colorRef' _ -> do
      color <- readIORef colorRef'
      pure $ Just (color, colorRef')
  uncleColor <- case mUncle of
    Nothing -> pure Nothing
    Just Empty -> pure Nothing
    Just (Node _ _ colorRef' _) -> do
      color <- readIORef colorRef'
      pure $ Just (color, colorRef')
  case (parentColor, uncleColor) of
    (Nothing, _) -> writeIORef colorRef Black
    (Just (Black, _), _) -> pure ()
    (Just (_, parentColorRef), Just (Red, uncleColorRef)) -> do
      mGrandparent <- grandparent node
      writeIORef parentColorRef Black
      writeIORef uncleColorRef Black
      case mGrandparent of
        Nothing -> pure ()
        Just grandparentNode@(Node _ _ grandparentColorRef _) -> do
          writeIORef grandparentColorRef Red
          insertRepair grandparentNode
    _ -> do
      nodeRef <- newIORef node
      parentRef' <- newIORef parent
      node' <- readIORef nodeRef
      parent' <- readIORef parentRef'
      mGrandparent <- grandparent node'
      case (node', parent', mGrandparent) of
        (_, _, Nothing) -> error "Invariant violated"
        (Node  _ _ _ (Links _ nodeLeftRef nodeRightRef), 
         parentNode@(Node _ _ _ (Links _ parentLeftRef parentRightRef)), 
         Just grandparentNode@(Node _ _ _ (Links _ grandparentLeftRef grandparentRightRef))) -> do
          parentLeft <- readIORef parentLeftRef
          parentRight <- readIORef parentRightRef
          grandparentLeft <- readIORef grandparentLeftRef
          grandparentRight <- readIORef grandparentRightRef
          if node' == parentRight && parent' == grandparentLeft
          then do
            rotateLeft parent'
            nodeLeft <- readIORef nodeLeftRef
            writeIORef nodeRef nodeLeft
          else when (node' == parentLeft && parent' == grandparentRight) $ do
            rotateRight parent'
            nodeRight <- readIORef nodeRightRef
            writeIORef nodeRef nodeRight
          node'' <- readIORef nodeRef
          unless (node'' == Empty) $ do
            let parentRef'' = case node'' of
                                Empty -> error "Impossible"
                                Node _ _ _ (Links ref _ _) -> ref 
            grandparentRef'' <- newIORef =<< grandparent node''
            parent'' <- readIORef parentRef''
            mGrandparent' <- readIORef grandparentRef''
            case (parent'', mGrandparent') of
              (Node _ _ parentColorRef (Links _ parentLeftRef _), Just grandparentNode@(Node _ _ grandparentColorRef _)) -> do
                parentLeft <- readIORef parentLeftRef
                if node'' == parentLeft
                then rotateRight grandparentNode
                else rotateLeft grandparentNode
                writeIORef parentColorRef Black
                writeIORef grandparentColorRef Red
            

rotateLeft :: RBTreeNode -> IO ()
rotateLeft Empty = pure ()
rotateLeft node@(Node _ _ _ (Links parentRef leftRef rightRef)) = do
  parent <- readIORef parentRef
  left <- readIORef leftRef
  right <- readIORef rightRef
  newNodeRef <- newIORef right
  newParentRef <- newIORef parent
  newNode@(Node _ _ _ (Links newNodeParentRef newNodeLeftRef _)) <- readIORef newNodeRef
  newNodeLeft <- readIORef newNodeLeftRef
  writeIORef rightRef newNodeLeft
  writeIORef newNodeLeftRef node
  writeIORef parentRef newNode
  right' <- readIORef rightRef
  case right' of
    Empty -> pure ()
    Node _ _ _ (Links parentRef' _ _) -> writeIORef parentRef' node
  newParent <- readIORef newParentRef
  case newParent of
    Empty -> pure ()
    Node _ _ _ (Links _ pLeftRef pRightRef) -> do
      pLeft <- readIORef pLeftRef
      pRight <- readIORef pRightRef
      if node == pLeft
      then writeIORef pLeftRef newNode
      else when (node == pRight) 
                (writeIORef pRightRef newNode)
  newParent' <- readIORef newParentRef
  writeIORef newNodeParentRef newParent'

rotateRight :: RBTreeNode -> IO ()
rotateRight Empty = pure ()
rotateRight node@(Node _ _ _ (Links parentRef leftRef rightRef)) = do
  parent <- readIORef parentRef
  right <- readIORef rightRef
  left <- readIORef leftRef
  newNodeRef <- newIORef left
  newParentRef <- newIORef parent
  newNode@(Node _ _ _ (Links newNodeParentRef _ newNodeRightRef)) <- readIORef newNodeRef
  newNodeRight <- readIORef newNodeRightRef
  writeIORef leftRef newNodeRight
  writeIORef newNodeRightRef node
  writeIORef parentRef newNode
  left' <- readIORef leftRef
  case left' of
    Empty -> pure ()
    Node _ _ _ (Links parentRef' _ _) -> writeIORef parentRef' node
  newParent <- readIORef newParentRef
  case newParent of
    Empty -> pure ()
    Node _ _ _ (Links _ pLeftRef pRightRef) -> do
      pLeft <- readIORef pLeftRef
      pRight <- readIORef pRightRef
      if node == pLeft
      then writeIORef pLeftRef newNode
      else when (node == pRight) 
                (writeIORef pRightRef newNode)
  newParent' <- readIORef newParentRef
  writeIORef newNodeParentRef newParent'
 

------ Tests

inputs :: Word8 -> [(Key, Value)]
inputs 0 = []
inputs n = (Key $ BS.singleton n, Value $ BS.replicate 10 n) : inputs (pred n)

testTree :: [(Key, Value)] -> IO RBTree 
testTree inputs = do
  tree <- empty
  for_ inputs (uncurry $ insert tree)
  pure tree
