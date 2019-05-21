module Sheet2.Ex4 where

type Title = String
type Artist = String
type Duration = Int
data Track = Track
    { title :: Title
    , artist :: Artist
    , duration :: Duration
    , rating :: [(User, Rating)]
    } deriving Show
instance Eq Track where
    t1 == t2 = title t1 == title t2 && artist t1 == artist t2
data Album = Album
    { name :: String
    , tracks :: [Track]
    } deriving Show
instance Eq Album where
    a1 == a2 = name a1 == name a2
data Rating = Bad | Good deriving (Show, Eq, Ord, Read)
type User = String
type MediaBib = [Album]

album1 = Album { name = "Battle Metal", tracks =
    [ Track "Victoriae & Triumphi Dominus" "Turisas" 87 []
    , Track "As Torches Rise" "Turisas" 291 []
    , Track "Battle Metal" "Turisas" 269 []
    ] }
noAlbum = Album { name = "", tracks =
    [ Track "Bad Apple" "Nomico" 324 [] 
    , Track "World Is Mine" "Hatsune Miku" 172 []
    ] }
myMediaBib = [album1, noAlbum]
track4 = Track "The Land of Hope and Glory" "Turisas" 382 []

addAlbum :: Track -> Album -> MediaBib -> MediaBib
addAlbum newTrack album = map addTrackToAlbum
    where addTrackToAlbum a@(Album name tracks) = if a == album then Album name (newTrack : tracks) else a

rateTrack :: User -> Track -> Rating -> MediaBib -> MediaBib
rateTrack user track rating = map rateTrackInAlbum
    where
        rateTrackInAlbum (Album name tracks) = Album name (map rateTrack tracks)
        rateTrack t@(Track tit art dur rat) = if t == track then Track tit art dur ((user, rating):rat) else t

getDurationsGroupedByAlbum :: MediaBib -> [(Album, Duration)]
getDurationsGroupedByAlbum = map getDuration

getDuration :: Album -> (Album, Duration)
getDuration a@(Album _ tracks) = (a, sum $ map duration tracks)
