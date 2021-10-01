cup fl0z = \f -> f fl0z
-- coffeeCup = cup 12

getOz aCup = aCup (\fl0z -> fl0z)
-- getOz coffeeCup => 12

drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
  where fl0z = getOz aCup
        ozDiff = fl0z - ozDrank
-- afterASip = drink coffeeCup 1
-- getOz afterASip => 11

isEmpty aCup = getOz aCup == 0

-- usage
{-
  afterBigGulp = drink coffeeCup 20
  getOz afterBigGulp -- 0

  afterManySips = foldl drink coffeeCup [1,1,1,1,1]
  getOz afterManySips -- 7
-}
