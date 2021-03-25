module RBTree (
  RBTree,
  empty,
  toList,
  search,
  insert
) where

import Types (Key (..), Value (..))
import Data.IORef
import Data.Word
import Data.Foldable (for_)
import Control.Monad (when, unless)
import qualified Data.ByteString as BS

data Color = Red | Black deriving (Eq, Show)

data Links = Links { parent :: IORef (Maybe RBTreeNode)
                   , left :: IORef (Maybe RBTreeNode)
                   , right :: IORef (Maybe RBTreeNode) }

data RBTreeNode = Node { nodeKey :: Key
                       , nodeValue :: IORef Value
                       , nodeColor :: IORef Color
                       , nodeLinks :: Links }

newtype RBTree = RBTree (IORef (Maybe RBTreeNode))

instance Eq RBTreeNode where
  Node key _ _ _ == Node key' _ _ _ = key == key'

grandparentNode :: Maybe RBTreeNode -> IO (Maybe RBTreeNode)
grandparentNode Nothing = pure Nothing
grandparentNode (Just (Node _ _ _ (Links parentRef _ _))) = do
  parent <- readIORef parentRef
  case parent of
    Nothing -> pure Nothing
    Just (Node _ _ _ (Links grandparentRef _ _)) -> do
      grandparent <- readIORef grandparentRef
      pure grandparent

uncleNode :: Maybe RBTreeNode -> IO (Maybe RBTreeNode)
uncleNode Nothing = pure Nothing
uncleNode justNode@(Just (Node _ _ _ (Links parentRef _ _))) = do
  parent <- readIORef parentRef
  mGrandparent <- grandparentNode justNode
  case (parent, mGrandparent) of
    (parentNode, Just (Node _ _ _ (Links _ leftRef rightRef))) -> do
      left <- readIORef leftRef
      right <- readIORef rightRef
      pure $ if left == parentNode then right else left
    _ -> pure Nothing

empty :: IO RBTree
empty = RBTree <$> newIORef Nothing

toList :: RBTree -> IO [(Key, Value)]
toList (RBTree rootRef) = readIORef rootRef >>= toList'
  where 
    toList' :: Maybe RBTreeNode -> IO [(Key, Value)]
    toList' Nothing = pure []
    toList' (Just (Node k valueRef _ (Links _ leftRef rightRef))) = 
      do
        left <- readIORef leftRef
        right <- readIORef rightRef
        value <- readIORef valueRef
        tail <- toList' left
        let tail' = tail ++ [(k, value)]
        rightTail <- toList' right
        pure $ tail' ++ rightTail

search :: RBTree -> Key -> IO (Maybe Value)
search (RBTree rootRef) key = readIORef rootRef >>= search' key
  where
    search' :: Key -> Maybe RBTreeNode -> IO (Maybe Value)
    search' _ Nothing = pure Nothing
    search' searchKey (Just (Node key valueRef _ (Links _ leftRef rightRef))) = do
      case compare searchKey key of
        LT -> readIORef leftRef >>= search' searchKey
        EQ -> readIORef valueRef >>= (pure . Just)
        GT -> readIORef rightRef >>= search' searchKey

mkNode :: Key -> Value -> Color -> Maybe RBTreeNode -> Maybe RBTreeNode -> Maybe RBTreeNode -> IO RBTreeNode
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
findRoot node@(Node _ _ _ (Links parentRef _ _)) = do
  mParent <- readIORef parentRef
  case mParent of
    Nothing -> pure node
    Just parent -> findRoot parent

insert :: RBTree -> Key -> Value -> IO ()
insert (RBTree rootRef) k v = do
  mRoot <- readIORef rootRef
  case mRoot of
    Nothing -> writeIORef rootRef =<< pure . Just =<< mkNode k v Black Nothing Nothing Nothing
    node -> do
      mNewNode <- insert' node k v
      case mNewNode of
        Nothing -> pure ()
        Just newNode -> do
          newRoot <- findRoot newNode
          writeIORef rootRef $ Just newRoot
  where
    insert' :: Maybe RBTreeNode -> Key -> Value -> IO (Maybe RBTreeNode)
    insert' (Just root@(Node key valueRef _ (Links _ leftRef rightRef))) newKey newValue = do
      case compare newKey key of
        EQ -> do
          writeIORef valueRef newValue
          pure Nothing
        LT -> do
          left <- readIORef leftRef
          case left of
            Nothing -> do
              newNode <- mkNode newKey newValue Red (Just root) Nothing Nothing
              writeIORef leftRef (Just newNode)
              insertRepair newNode
              pure $ Just newNode
            node -> insert' node newKey newValue
        GT -> do
          right <- readIORef rightRef
          case right of
            Nothing -> do
              newNode <- mkNode newKey newValue Red (Just root) Nothing Nothing
              writeIORef rightRef (Just newNode)
              insertRepair newNode
              pure $ Just newNode
            node -> insert' node newKey newValue

insertRepair :: RBTreeNode -> IO ()
insertRepair node@(Node _ _ colorRef (Links parentRef leftRef rightRef)) = do
  parent <- readIORef parentRef
  mUncle <- uncleNode $ Just node
  parentColor <- case parent of
    Nothing -> pure Nothing
    Just (Node _ _ colorRef' _) -> do
      color <- readIORef colorRef'
      pure $ Just (color, colorRef')
  uncleColor <- case mUncle of
    Nothing -> pure Nothing
    Just (Node _ _ colorRef' _) -> do
      color <- readIORef colorRef'
      pure $ Just (color, colorRef')
  case (parentColor, uncleColor) of
    (Nothing, _) -> writeIORef colorRef Black
    (Just (Black, _), _) -> pure ()
    (Just (_, parentColorRef), Just (Red, uncleColorRef)) -> do
      mGrandparent <- grandparentNode $ Just node
      writeIORef parentColorRef Black
      writeIORef uncleColorRef Black
      case mGrandparent of
        Nothing -> pure ()
        Just grandparentNode@(Node _ _ grandparentColorRef _) -> do
          writeIORef grandparentColorRef Red
          insertRepair grandparentNode
    _ -> do
      nodeRef <- newIORef $ pure node
      parentRef' <- newIORef parent
      node' <- readIORef nodeRef
      parent' <- readIORef parentRef'
      mGrandparent <- grandparentNode $ Just node
      case (node', parent', mGrandparent) of
        (_, _, Nothing) -> error "Invariant violated"
        (Just (Node  _ _ _ (Links _ nodeLeftRef nodeRightRef)), 
         Just (Node _ _ _ (Links _ parentLeftRef parentRightRef)), 
         Just (Node _ _ _ (Links _ grandparentLeftRef grandparentRightRef))) -> do
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
          unless (node'' == Nothing) $ do
            let parentRef'' = case node'' of
                                Nothing -> error "Impossible"
                                Just (Node _ _ _ (Links ref _ _)) -> ref 
            grandparentRef'' <- newIORef =<< grandparentNode node''
            parent'' <- readIORef parentRef''
            mGrandparent' <- readIORef grandparentRef''
            case (parent'', mGrandparent') of
              (Just (Node _ _ parentColorRef (Links _ parentLeftRef _)), grandparent@(Just (Node _ _ grandparentColorRef _))) -> do
                parentLeft <- readIORef parentLeftRef
                if node'' == parentLeft
                then rotateRight grandparent
                else rotateLeft grandparent
                writeIORef parentColorRef Black
                writeIORef grandparentColorRef Red
            

rotateLeft :: Maybe RBTreeNode -> IO ()
rotateLeft Nothing = pure ()
rotateLeft justNode@(Just (Node _ _ _ (Links parentRef leftRef rightRef))) = do
  parent <- readIORef parentRef
  left <- readIORef leftRef
  right <- readIORef rightRef
  newNodeRef <- newIORef right
  newParentRef <- newIORef parent
  newNode@(Just (Node _ _ _ (Links newNodeParentRef newNodeLeftRef _))) <- readIORef newNodeRef
  newNodeLeft <- readIORef newNodeLeftRef
  writeIORef rightRef newNodeLeft
  writeIORef newNodeLeftRef justNode
  writeIORef parentRef newNode
  right' <- readIORef rightRef
  case right' of
    Nothing -> pure ()
    Just (Node _ _ _ (Links parentRef' _ _)) -> writeIORef parentRef' justNode
  newParent <- readIORef newParentRef
  case newParent of
    Nothing -> pure ()
    Just (Node _ _ _ (Links _ pLeftRef pRightRef)) -> do
      pLeft <- readIORef pLeftRef
      pRight <- readIORef pRightRef
      if justNode == pLeft
      then writeIORef pLeftRef newNode
      else when (justNode == pRight) 
                (writeIORef pRightRef newNode)
  newParent' <- readIORef newParentRef
  writeIORef newNodeParentRef newParent'

rotateRight :: Maybe RBTreeNode -> IO ()
rotateRight Nothing = pure ()
rotateRight justNode@(Just (Node _ _ _ (Links parentRef leftRef rightRef))) = do
  parent <- readIORef parentRef
  right <- readIORef rightRef
  left <- readIORef leftRef
  newNodeRef <- newIORef left
  newParentRef <- newIORef parent
  newNode@(Just (Node _ _ _ (Links newNodeParentRef _ newNodeRightRef))) <- readIORef newNodeRef
  newNodeRight <- readIORef newNodeRightRef
  writeIORef leftRef newNodeRight
  writeIORef newNodeRightRef justNode
  writeIORef parentRef newNode
  left' <- readIORef leftRef
  case left' of
    Nothing -> pure ()
    Just (Node _ _ _ (Links parentRef' _ _)) -> writeIORef parentRef' justNode
  newParent <- readIORef newParentRef
  case newParent of
    Nothing -> pure ()
    Just (Node _ _ _ (Links _ pLeftRef pRightRef)) -> do
      pLeft <- readIORef pLeftRef
      pRight <- readIORef pRightRef
      if justNode == pLeft
      then writeIORef pLeftRef newNode
      else when (justNode == pRight) 
                (writeIORef pRightRef newNode)
  newParent' <- readIORef newParentRef
  writeIORef newNodeParentRef newParent'
