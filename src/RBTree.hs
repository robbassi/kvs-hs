{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RBTree
  ( RBTree,
    empty,
    toList,
    search,
    insert,
  )
where

import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Types (Key (..), Value (..))

data Color = Red | Black deriving (Eq, Show)

data RBTreeNode = Node
  { key :: Key,
    valueRef :: IORef Value,
    colorRef :: IORef Color,
    parentRef :: IORef (Maybe RBTreeNode),
    leftRef :: IORef (Maybe RBTreeNode),
    rightRef :: IORef (Maybe RBTreeNode)
  }

newtype RBTree = RBTree (IORef (Maybe RBTreeNode))

instance Eq RBTreeNode where
  Node key _ _ _ _ _ == Node key' _ _ _ _ _ = key == key'

grandparentNode :: Maybe RBTreeNode -> IO (Maybe RBTreeNode)
grandparentNode Nothing = pure Nothing
grandparentNode (Just Node {..}) = do
  parent <- readIORef parentRef
  case parent of
    Nothing -> pure Nothing
    Just (Node _ _ _ grandparentRef _ _) -> readIORef grandparentRef

uncleNode :: Maybe RBTreeNode -> IO (Maybe RBTreeNode)
uncleNode Nothing = pure Nothing
uncleNode justNode@(Just Node {..}) = do
  parent <- readIORef parentRef
  mGrandparent <- grandparentNode justNode
  case (parent, mGrandparent) of
    (parentNode, Just Node {..}) -> do
      left <- readIORef leftRef
      right <- readIORef rightRef
      pure $ if left == parentNode then right else left
    _ -> pure Nothing

empty :: IO RBTree
empty = RBTree <$> newIORef Nothing

toList :: RBTree -> IO [(Key, Value)]
toList (RBTree rootRef) = toList' =<< readIORef rootRef
  where
    toList' :: Maybe RBTreeNode -> IO [(Key, Value)]
    toList' Nothing = pure []
    toList' (Just Node {..}) =
      do
        left <- readIORef leftRef
        right <- readIORef rightRef
        value <- readIORef valueRef
        tail <- toList' left
        let tail' = tail ++ [(key, value)]
        rightTail <- toList' right
        pure $ tail' ++ rightTail

search :: RBTree -> Key -> IO (Maybe Value)
search (RBTree rootRef) searchKey = search' =<< readIORef rootRef
  where
    search' :: Maybe RBTreeNode -> IO (Maybe Value)
    search' Nothing = pure Nothing
    search' (Just Node {..}) = do
      case compare searchKey key of
        LT -> search' =<< readIORef leftRef
        EQ -> Just <$> readIORef valueRef
        GT -> search' =<< readIORef rightRef

mkNode :: Key -> Value -> Color -> Maybe RBTreeNode -> Maybe RBTreeNode -> Maybe RBTreeNode -> IO RBTreeNode
mkNode key value color parentNode leftNode rightNode = do
  valueRef <- newIORef value
  colorRef <- newIORef color
  parentRef <- newIORef parentNode
  leftRef <- newIORef leftNode
  rightRef <- newIORef rightNode
  pure $ Node {..}

findRoot :: RBTreeNode -> IO RBTreeNode
findRoot node@Node {..} = do
  mParent <- readIORef parentRef
  case mParent of
    Nothing -> pure node
    Just parent -> findRoot parent

insert :: RBTree -> Key -> Value -> IO ()
insert (RBTree rootRef) newKey newValue = do
  mRoot <- readIORef rootRef
  case mRoot of
    Nothing -> writeIORef rootRef =<< (Just <$> mkNode newKey newValue Black Nothing Nothing Nothing)
    node -> do
      mNewNode <- insert' node
      case mNewNode of
        Nothing -> pure ()
        Just newNode -> do
          newRoot <- findRoot newNode
          writeIORef rootRef $ Just newRoot
  where
    insert' :: Maybe RBTreeNode -> IO (Maybe RBTreeNode)
    insert' Nothing = pure Nothing
    insert' (Just root@Node {..}) = do
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
            node -> insert' node
        GT -> do
          right <- readIORef rightRef
          case right of
            Nothing -> do
              newNode <- mkNode newKey newValue Red (Just root) Nothing Nothing
              writeIORef rightRef (Just newNode)
              insertRepair newNode
              pure $ Just newNode
            node -> insert' node

insertRepair :: RBTreeNode -> IO ()
insertRepair node@Node {..} = do
  parent <- readIORef parentRef
  mUncle <- uncleNode $ Just node
  parentColor <- case parent of
    Nothing -> pure Nothing
    Just (Node _ _ colorRef' _ _ _) -> do
      color <- readIORef colorRef'
      pure $ Just (color, colorRef')
  uncleColor <- case mUncle of
    Nothing -> pure Nothing
    Just (Node _ _ colorRef' _ _ _) -> do
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
        Just grandparentNode@(Node _ _ grandparentColorRef _ _ _) -> do
          writeIORef grandparentColorRef Red
          insertRepair grandparentNode
    _ -> do
      nodeRef <- newIORef $ pure node
      parentRef' <- newIORef parent
      node' <- readIORef nodeRef
      parent' <- readIORef parentRef'
      mGrandparent <- grandparentNode $ Just node
      case (node', parent', mGrandparent) of
        ( Just (Node _ _ _ _ nodeLeftRef nodeRightRef),
          Just (Node _ _ _ _ parentLeftRef parentRightRef),
          Just (Node _ _ _ _ grandparentLeftRef grandparentRightRef)
          ) -> do
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
            unless (isNothing node'') $ do
              let parentRef'' = case node'' of
                    Nothing -> error "Impossible"
                    Just Node {..} -> parentRef
              grandparentRef'' <- newIORef =<< grandparentNode node''
              parent'' <- readIORef parentRef''
              mGrandparent' <- readIORef grandparentRef''
              case (parent'', mGrandparent') of
                (Just (Node _ _ parentColorRef _ parentLeftRef _), grandparent@(Just (Node _ _ grandparentColorRef _ _ _))) -> do
                  parentLeft <- readIORef parentLeftRef
                  if node'' == parentLeft
                    then rotateRight grandparent
                    else rotateLeft grandparent
                  writeIORef parentColorRef Black
                  writeIORef grandparentColorRef Red
                _ -> error "Invariant violated"
        _ -> error "Invariant violated"

rotateLeft :: Maybe RBTreeNode -> IO ()
rotateLeft Nothing = pure ()
rotateLeft justNode@(Just Node {..}) = do
  parent <- readIORef parentRef
  right <- readIORef rightRef
  newNodeRef <- newIORef right
  newParentRef <- newIORef parent
  newNode@(Just (Node _ _ _ newNodeParentRef newNodeLeftRef _)) <- do
    mNode <- readIORef newNodeRef
    case mNode of
      Nothing -> error "Invariant violated"
      node -> pure node
  newNodeLeft <- readIORef newNodeLeftRef
  writeIORef rightRef newNodeLeft
  writeIORef newNodeLeftRef justNode
  writeIORef parentRef newNode
  right' <- readIORef rightRef
  case right' of
    Nothing -> pure ()
    Just (Node _ _ _ parentRef' _ _) -> writeIORef parentRef' justNode
  newParent <- readIORef newParentRef
  case newParent of
    Nothing -> pure ()
    Just (Node _ _ _ _ pLeftRef pRightRef) -> do
      pLeft <- readIORef pLeftRef
      pRight <- readIORef pRightRef
      if justNode == pLeft
        then writeIORef pLeftRef newNode
        else
          when
            (justNode == pRight)
            (writeIORef pRightRef newNode)
  newParent' <- readIORef newParentRef
  writeIORef newNodeParentRef newParent'

rotateRight :: Maybe RBTreeNode -> IO ()
rotateRight Nothing = pure ()
rotateRight justNode@(Just Node {..}) = do
  parent <- readIORef parentRef
  left <- readIORef leftRef
  newNodeRef <- newIORef left
  newParentRef <- newIORef parent
  newNode@(Just (Node _ _ _ newNodeParentRef _ newNodeRightRef)) <- do
    mNode <- readIORef newNodeRef
    case mNode of
      Nothing -> error "Invariant violated"
      node -> pure node
  newNodeRight <- readIORef newNodeRightRef
  writeIORef leftRef newNodeRight
  writeIORef newNodeRightRef justNode
  writeIORef parentRef newNode
  left' <- readIORef leftRef
  case left' of
    Nothing -> pure ()
    Just (Node _ _ _ parentRef' _ _) -> writeIORef parentRef' justNode
  newParent <- readIORef newParentRef
  case newParent of
    Nothing -> pure ()
    Just (Node _ _ _ _ pLeftRef pRightRef) -> do
      pLeft <- readIORef pLeftRef
      pRight <- readIORef pRightRef
      if justNode == pLeft
        then writeIORef pLeftRef newNode
        else
          when
            (justNode == pRight)
            (writeIORef pRightRef newNode)
  newParent' <- readIORef newParentRef
  writeIORef newNodeParentRef newParent'
