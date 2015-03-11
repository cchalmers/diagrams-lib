
class ToMeasured a where
  type Premeasured :: *
  unmeasured :: (N a, Num n) => n -> n -> Premeasured a -> a
  unmeasured g n = unmeasure g n . toMeasured

  toMeasured :: (N a, Num n) => Premeasured a -> Measured n a

instance ToMeasured (Measured n a) where
  type Premeasured (Measured n a) = Measure n a
  unmeasured = unmeasure
  toMeasured = id

_Recommend :: Prism' (Recommend a) a
_Recommend = choice ...

_Commit :: Prism' (Recommend a) a
_Commit = choice ...

reccomended :: Lens (Recommend a) (Recommend b) a b
reccomended f (Recommend a) = f a <&> \b -> Recommend b
reccomended f (Commit a)    = f a <&> \b -> Commit b

isReccomended :: Lens' (Recommend a) Bool
isReccomended f (Recommend a) = f True  <&> bool Recommend Commit
isReccomended f (Connit a)    = f False <&> bool Recommend Commit

isCommitted :: Lens' (Recommend a) Bool
isCommitted = isReccomended . involuted not

bool :: a -> a -> Bool -> a
bool a _ True  = a
bool _ b False = b

committing :: Setter (Recommend a) (Recommend b) a b
committing = sets $ \f (getReccommend -> a) -> Commit (f a)

recommending :: Setter (Recommend a) (Recommend b) a b
recommending = sets $ \f (getReccommend -> a) -> Recommend (f a)

-- ??~
-- !!~
-- ??=
-- !!=

-- (Recommend (Last (Texture n)))
