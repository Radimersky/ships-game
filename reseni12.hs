-- IB015 2019 - Kostra řešení dvanácté domácí úlohy
--   * V kostře nahraďte ‚undefined‘ vlastní implementací.
--   * Definicím funkcí můžete přidávat formální parametry.
--   * DULEŽITÉ: Zadané datové typy nemodifikujte.
--   * DŮLEŽITÉ: Zadaným funkcím neměňte typové signatury.
--   * DŮLEŽITÉ: Ke všem globálně definovaným funkcím uvádějte typovou signaturu.
--   * Řešení si zkuste spustit na Aise s GHC 8.6.
--   * Vyřešenou úlohu nahrajte do odevzdávárny své seminární skupiny.
-- Před tento řádek nic nepřidávejte
import Text.Read
import Data.List (find, words)
import Data.Maybe (isNothing, fromJust)

---------------------------------------------------
--      Z A D A N É   D A T O V É   T Y P Y      --
---------------------------------------------------

-- loď se seznamem příslušných koordinátů s pravdivostní hodnotou
-- podle toho, zda jsou zasaženy (True = zasažen, False = nezasažen)
data Ship = Ship [(Coord, Status)]
          deriving (Eq, Show)

-- moře jsou čtvercová
data ShipsPlan = ShipPlan Int [Ship]
               deriving (Eq, Show)

type Coord = (Int, Int)

data ShipOrientation = Horizontal
                     | Vertical
                     deriving Show

type ShipSize = Int

data Status = AsNew
            | Damaged
            deriving (Eq, Show)

data ShotResult = Ocean
                | Hit
                | Sunk
                deriving (Show, Eq)


---------------------------------------------------
--   F U N K C E   K   I M P L E M E N T A C I   --
---------------------------------------------------

isEmpty :: ShipsPlan -> Bool
isEmpty (ShipPlan _ []) = True
isEmpty _ = False


getSize :: ShipsPlan -> Int
getSize (ShipPlan size _) = size

toShip :: Coord -> ShipOrientation -> ShipSize -> Ship
toShip (x, y) Horizontal size = Ship [ ((x', y), AsNew) | x' <- [x..(x + size - 1)] ]
toShip (x, y) Vertical   size = Ship [ ((x, y'), AsNew) | y' <- [y..(y + size - 1)] ]


placeShip :: Coord -> ShipOrientation -> ShipSize -> ShipsPlan -> Maybe ShipsPlan
placeShip crd orient size plan = if size <= 0 || overflows orient planSize size crd || collides plan ship
                                 then Nothing
                                 else Just $ addShip plan ship
    where 
          addShip (ShipPlan sz ships) shp = ShipPlan sz $ shp : ships
          overflows Vertical   sizeP sizeS (x, y) = not $ x > 0 && y > 0 && x <= sizeP && sizeP >= y + sizeS - 1
          overflows Horizontal sizeP sizeS (x, y) = not $ x > 0 && y > 0 && y <= sizeP && sizeP >= x + sizeS - 1
          planSize = getSize plan
          ship     = toShip crd orient size
          collides (ShipPlan _ ships) (Ship list) = any (\e -> e `elem` occupied) coordinates
              where occupied = concat $ map (\(Ship lst) -> map fst lst) ships
                    coordinates = map fst list


isDamaged :: Status -> Bool
isDamaged Damaged = True
isDamaged _       = False


takeHit :: Coord -> ShipsPlan -> Ship -> (ShipsPlan, ShotResult)
takeHit crd (ShipPlan size ships) sh@(Ship list) = (newPlan, result)
    where
          damagedShip = Ship $ map (\(c, st) -> if crd == c then (c, Damaged) else (c, st)) list
          (Ship damaged) = damagedShip
          isDestroyed = all (isDamaged . snd) damaged
          result = if isDestroyed then Sunk else Hit
          newPlan = ShipPlan size $ removeOrUpdate isDestroyed sh damagedShip ships
              where
                    removeOrUpdate False ship new shs = map (\s -> if s == ship then new else s) shs
                    removeOrUpdate True  ship _   shs = filter (/= ship) shs


shoot :: Coord -> ShipsPlan -> (ShipsPlan, ShotResult)
shoot crd p@(ShipPlan _ ships) = if isNothing target then (p, Ocean) else takeHit crd p t
    where
          target = find (\(Ship list) -> any ((== crd) . fst) list) ships
          (Just t) = target


getSym :: Coord -> [Ship] -> Char
getSym crd ships = if damaged then 'X' else '#'
    where (Just ship) = find (\(Ship list) -> crd `elem` map fst list) ships
          (Ship coorB) = ship
          damaged = isDamaged $ snd $ head $ filter ((== crd) . fst) coorB


printLines :: [String] -> IO ()
printLines = putStr . unlines

printPlan :: ShipsPlan -> IO ()
printPlan (ShipPlan size ships) = printLines $ groupBy size plan
    where
          shipsCrd = concat $ map (\(Ship coorB) -> map fst coorB) ships
          plan = [ symbol | y <- [1..size], x <- [1..size], let symbol = if (x, y) `elem` shipsCrd
                                                                         then getSym (x, y) ships else '~' ]
          groupBy _ [] = []
          groupBy n list = take n list : groupBy n (drop n list)



phase1 :: IO ShipsPlan
phase1 = do
         putStrLn "Enter plan size "
         str_x <- getLine
         let x = readMaybe str_x :: Maybe Int
         if isNothing x || fromJust x < 1
         then do
              putStrLn "Invalid input"
              phase1
         else pure $ ShipPlan (fromJust x) []


loadShip :: String -> ShipsPlan -> IO ShipsPlan
loadShip input plan = do
                if isNothing shipInfoParsed
                then do
                     putStrLn "Invalid ship input! Try again."
                     phase2 plan
                else do
                     let res = placeShip crd orient size plan
                     if isNothing res
                     then do
                          putStrLn "Could not place the ship! Try again."
                          phase2 plan
                     else do
                          let newPlan = fromJust res
                          printPlan newPlan
                          phase2 newPlan
    where shipInfoParsed = parseShipInput input
          Just (crd, orient, size) = shipInfoParsed

phase2 :: ShipsPlan -> IO ShipsPlan
phase2 plan = do
              putStrLn "Enter ship: X Y [V | H] SIZE    (or end)."
              input <- getLine
              if input == "end"
              then pure plan
              else loadShip input plan


checkAndShoot :: ShipsPlan -> Maybe Coord -> IO (ShipsPlan, Bool)
checkAndShoot plan Nothing      = putStrLn "Invalid input! Try again." >> pure (plan, False)
checkAndShoot plan (Just (x, y)) = do
                                   if isInvalid then checkAndShoot plan Nothing
                                   else do
                                        let (newPlan, result) = shoot (x, y) plan
                                        putStrLn $ show result
                                        pure (newPlan, True)
    where
          isInvalid = isOutside (x, y) (getSize plan)
          isOutside (x', y') sz = x' <= 0 || y' <= 0 || x' > sz || y' > sz


phase3 :: ShipsPlan -> IO ()
phase3 plan = do
              putStrLn "Shoot to X Y    (or enter end)."
              input <- getLine
              if input == "end"
              then putStrLn "Ending..."
              else do
                   (newPlan, validInput) <- checkAndShoot plan (parseShootInput input)
                   if validInput && isEmpty newPlan then putStrLn "You won!"
                                                    else phase3 newPlan



game :: IO ()
game = do
       emptyPlan <- phase1
       plan <- phase2 emptyPlan
       printPlan plan
       phase3 plan


---------------------------------------------------
--         P O M O C N É   F U N K C E           --
---------------------------------------------------

-- Pomocná funkce pro zpracování řádku načteného pro zadání lodě.
-- Funkce řeší pouze zpracování řádku zadávající loď a v případě,
-- že je tento vstup validní, vrací zpracované parametry zabalené
-- v Maybe. V opačném případě vrací Nothing.
-- Možná vás překvapí do-notace bez IO. Ve skutečnosti tu využíváme
-- toho, že Maybe je stejně jako IO tzv. monádou - podrobnosti pře-
-- sahují rámec tohoto kurzu. Nám stačí vědět, že (stejně jako u IO)
-- pokud nějaký z výpočtů selže (takže funkce z níž si vytahujeme
-- hodnotu pomocí "<-" vrátí Nothing), tak selže celá funkce jako
-- celek -> návratová hodnota bude Nothing. Můžete si zkusit volání
-- vyhodnotit:   parseShipInput "3 4 A 10"
-- (Výsledkem bude Nothing - selže parseOrientation. Všimněte si, že
-- není potřeba po každém volání kontrolovat, zdali volání funkce
-- uspělo - o to se nám postará do-notace, resp. funkce (>>) a (>>=)).
parseShipInput :: String -> Maybe (Coord, ShipOrientation, ShipSize)
parseShipInput input = if length inputs /= 4 then Nothing
                       else do
                            x <- readMaybe str_x
                            y <- readMaybe str_y
                            orientation <- parseOrientation str_or
                            size <- readMaybe str_size
                            return ((x, y), orientation, size)
    where
          inputs = words input
          [str_x, str_y, str_or, str_size] = inputs
          parseOrientation "V" = Just Vertical
          parseOrientation "H" = Just Horizontal
          parseOrientation  _  = Nothing


-- Analogicky pomocná funkce pro zpracování řádku načteného pro zadání souřadnic
-- pro střelbu.
parseShootInput :: String -> Maybe Coord
parseShootInput input = if length inputs /= 2 then Nothing
                        else do
                             x <- readMaybe str_x
                             y <- readMaybe str_y
                             return (x, y)
    where inputs = words input
          [str_x, str_y] = inputs


-- při kompilaci pomocí `ghc zadani12.hs -o <jméno_výstupního_souboru>` a následném
-- spuštění výsledné binárky se nám automaticky zavolá funkce game
main :: IO ()
main = game

