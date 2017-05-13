module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
--import Control.Monad.Aff (launchAff)
--import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
--import Control.Monad.Eff.Exception (EXCEPTION)
--import Node.FS (FS)
--import Node.Encoding (Encoding(..))
--import Node.FS.Sync (readTextFile)
--import Node.FS.Aff (readTextFile)
--import Node.Path (FilePath)
--import Data.String.Utils (words)

import Data.Tuple ( Tuple(..) )
import Data.Array ( (!!), length, zip )
import Data.Foldable ( fold, traverse_ )
import Data.Maybe (Maybe (..), fromJust )
import Partial.Unsafe ( unsafePartial )
import Data.List ( List(..),  (:), mapMaybe )

import FV.Types
  ( VHMeas (..), HMeas (..), QMeas (..), PMeas (..)
  , XMeas (..), Prong (..), Chi2 (..)
  , vertex, helices, hFilter, fromHMeas, fromQMeas, vBlowup, invMass
  )

import Test.Input ( hSlurp, hSlurpMCtruth )
import Data.Matrix
  ( Matrix
  , identity, zero_, matrix, fromArray2
  , getElem, diagonal, subm2, submatrix
  , multStd
  , toLists, fromArrays, fromArray
  , nrows, ncols
  )
import Data.Cov
  ( Cov (..)
  , testCov, inv, invMaybe
  )
import FV.Fit ( fit )

import Data.Number ( fromString )
import Stuff

showMomentum :: forall e. HMeas -> Eff (console :: CONSOLE | e) Unit
showMomentum h = log $ "pt,pz,fi,E ->" <> (show <<< fromHMeas) h
showHelix :: forall e. HMeas -> Eff (console :: CONSOLE | e) Unit
showHelix h = log $ "Helix ->" <> (show h)

main :: forall e.  Eff ( console :: CONSOLE
                       --, exception :: EXCEPTION
                       --, fs :: FS
                       | e) Unit
--main = void $ launchAff do
main = do
  log "FVT Test Suite"
  {-- log "--Test hSlurp" --}
  {-- testHSlurp --}
  {-- log "--Test Matrix" --}
  {-- testMatrix --}
  log "--Test Cov"
  log $ testCov
  log "--Test FVT"
  testFVT
  {-- (VHMeas _ hl, _) <- hSlurp "dat/tav-0.dat" --}
  {-- let HMeas _ _ w = head hl --}
  {-- w `shouldBe` 0.0114 --}

testFVT :: forall e. Eff (console :: CONSOLE | e) Unit
testFVT = do
  let vm = unsafePartial fromJust $ hSlurp tr05129e001412
  let hel = helices vm
  traverse_ showHelix hel
  traverse_ showMomentum hel
  let l5 = [0,2,3,4,5] -- these are the tracks supposedly from the tau
  doFitTest vm l5
  {-- _ <- showProng <<< fitw <<< hFilter l5 <<< vBlowup 10000.0 $ vm --}

doFitTest :: forall e. VHMeas 
            -> Array Int
            -> Eff (console :: CONSOLE | e) Unit
doFitTest vm' l5 = do
  let vm = vBlowup 10000.0 vm'
  let showLen xs = show $ length xs
      showQChi2 :: forall e. (Tuple QMeas Chi2) -> Eff (console :: CONSOLE | e) Unit
      showQChi2 (Tuple qm (Chi2 chi2)) = log $ "q"
                                <> " chi2 ->" <> to1fix chi2
                                <> " pt,pz,fi,E ->"
                                <> show qm

  log $ "initial vertex position -> " <> show ((vertex vm)::XMeas)

  let pl         = map (fromQMeas <<< fromHMeas) $ helices vm
  log $ ("Inv Mass " <> showLen pl <> " helix") <> show (invMass pl)
  let pl5        = map (fromQMeas <<< fromHMeas) (helices <<< hFilter l5 $ vm)
  log $ ("Inv Mass " <> showLen pl5 <> " helix") <> show (invMass pl5)

  log             "Fitting Vertex --------------------"
  let pr = fit vm
      Prong {fitVertex: vf, fitMomenta: ql, fitChi2s: cl} = fit vm
  log $ "Fitted vertex -> " <> show vf
  traverse_ showQChi2 $ zip ql cl
  log $ "Inv Mass " <> show (length ql) <> " fit" 
                    <> show (invMass (map fromQMeas ql))

  {-- let m5 = invMass . map q2p . iflt l5 $ ql --}
  {--     iflt rng hl = [h | (h, i) <- zip hl [0..], i `elem` rng ] --}
  {-- putStrLn $ "Inv Mass " ++ showLen (iflt l5 ql) ++ " fit" ++ show m5 --}

  {-- putStrLn             "Refitting Vertex-----------------" --}
  {-- let prf = fit . hFilter l5 $ vm --}
  {-- putStrLn $ "Refitted vertex -> " ++ show (fitVertex prf) --}
  {-- mapM_ showQChi2 $ zip3 (fitMomenta prf) (fitChi2s prf) [0..] --}
  {-- putStrLn $ "Inv Mass " ++ (show $ nProng prf) ++ " refit" ++ show (invMass $ map q2p (fitMomenta prf)) --}
  {-- putStrLn $ "Final vertex -> " ++ show (fitVertex prf) --}
  log "end of doFitTest------------------------------------------"

testHSlurp :: forall e. Eff (console :: CONSOLE | e) Unit
testHSlurp = do
  log "Test hSlurp dat/tr05129e001412.dat"
  {-- ds <- readTextFile UTF8 "dat/tr05129e001412.dat" --}
  {-- let ws = mapMaybe fromString $ words ds --}
  let ws = tr05129e001412
  log "Test hSlurp dat/tav-4.dat"
  logShow $ mapMaybe fromString $ words tav4
  logShow $ hSlurp tav4
  logShow $ hSlurp ws
  --logShow =<< hSlurp tav4
  let mc = hSlurpMCtruth tav4
  logShow mc

testMatrix :: forall e. Eff (console :: CONSOLE | e) Unit
testMatrix = do
  log $ "Test identity 3"
  logShow $ identity 3
  log $ "Test zero 3 3"
  logShow $ zero_ 3 3
  log "Test Matrix operations: identity == zero?"
  logShow $ (identity 3) == (zero_ 3 3)
  log $ "Test matrix creation"
  let m0 = matrix 3 3 $ \(Tuple i j) -> 2*i - j
  logShow $ Tuple (m0 == (fromArray2 3 3 [1,0,-1,3,2,1,5,4,3])) m0
  let m1 = matrix 3 4 $ \(Tuple i j) -> 2*i - j
  logShow $ Tuple (m1 == (fromArray2 3 3 [1,0,-1,3,2,1,5,4,3])) m1

  log $ "Test getElem"
  let e1 = getElem 3 4 m1
  logShow $ Tuple (e1 == 2) e1

  log $ "Test fromArrays"
  let ll0 :: List (Array Int)
      ll0 = [1,0,0] : [0,2,0] : [0,0,3] : Nil
      ll1 = fromArrays ll0
  logShow $ Tuple (ll1 == diagonal 0 [1,2,3]) ll0

  log $ "Test toLists"
  let l0 = diagonal 0 [1,2,3]
      l1 = toLists l0 !! 2
  logShow $ Tuple (l1 == Just [0,0,3]) l1

  log $ "Test arithmetic"
  let m35 = fromArrays $ [1,2,3,4,5] : [2,3,4,5,6] : [3,4,5,6,7] : Nil
      m53 = fromArrays $ [1,0,0] : [0,1,0] : [0,0,1] : [0,0,0] : [0,0,0] : Nil
      d5n = diagonal 0.0 [2.0, 2.0, 2.0, 2.0, 2.0]
      d5 = diagonal 0 [2, 2, 2, 2, 2]
  logShow $ m35 * m53 == (fromArrays $ [1,2,3] : [2,3,4] : [3,4,5] : Nil)
  logShow $ m35 * m53
  logShow $ (diagonal 0 [1,1,1,1,1]) + (diagonal 0 [1,1,1,1,1]) == (diagonal 0 [2,2,2,2,2])
  logShow $ (fromArray2 3 3 [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]) * one
  logShow $ (fromArray2 3 3 [1,2,3,4,5,6,7,8,9]) * one
  logShow $ (fromArray2 3 3 [0,0,1,0,1,0,1,0,0]) * (fromArray2 3 3 [11, 12,13,21,22,23,31,32,33]) * one
  logShow $ (fromArray2 3 3 [11, 12,13,21,22,23,31,32,33]) * fromArray 3 [-1,1,1]

  {-- log "Test submatrix" --}
  {-- let xxms = fromArray2 5 5 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] --}
  {-- logShow $ submatrix 2 3 1 3 $ subm2 4 xxms --}

tr05129e001412 :: String
tr05129e001412 = """
   3.355679512023926       3.489715576171875       7.110095977783203    
  0.2884106636047363      0.2967556118965149      0.4457152485847473    
  0.2967556118965149      0.3057302236557007      0.4589158892631531    
  0.4457152485847473      0.4589158892631531      0.7007381319999695    
  4.5451703E-03
           6
  9.0513890609145164E-04   1.174186706542969      0.7913663387298584    
 -5.4129425436258316E-02   1.309153556823730    
  3.0409931517372257E-11  3.0817798313265143E-10 -2.6150961396353978E-09
 -6.2086684238238377E-08  1.9006475560079394E-10  3.0817798313265143E-10
  3.5358195873413933E-06 -5.5664237663677341E-09 -4.7704439509743679E-08
 -3.5389247932471335E-04 -2.6150961396353978E-09 -5.5664237663677341E-09
  3.9334932466772443E-07  9.2603177108685486E-06 -4.2692363422247581E-07
 -6.2086684238238377E-08 -4.7704439509743679E-08  9.2603177108685486E-06
  2.7857377426698804E-04 -1.2511900422396138E-05  1.9006475560079394E-10
 -3.5389247932471335E-04 -4.2692363422247581E-07 -1.2511900422396138E-05
  4.6403184533119202E-02
 -3.2948562875390053E-04  -1.287435531616211       3.964143753051758    
 -5.5920504033565521E-02   2.172087669372559    
  1.0773015292342425E-11  1.0870629917059116E-11 -9.4798713323740458E-10
 -2.6224558524745589E-08  5.1304871462320989E-10  1.0870629917059116E-11
  1.3991236755828140E-06  6.1739335865951261E-11  3.9363889925425610E-09
 -1.3362320896703750E-04 -9.4798713323740458E-10  6.1739335865951261E-11
  1.0642112613368226E-07  3.0040880574233597E-06 -5.7571856615368233E-08
 -2.6224558524745589E-08  3.9363889925425610E-09  3.0040880574233597E-06
  1.0815335554070771E-04 -1.6780244322944782E-06  5.1304871462320989E-10
 -1.3362320896703750E-04 -5.7571856615368233E-08 -1.6780244322944782E-06
  1.5890464186668396E-02
  8.6099491454660892E-04   1.190025329589844      0.7718949913978577    
  -1.004449844360352       4.974927902221680    
  7.8076378695612902E-10 -2.4755367200590683E-10 -1.0359136126680824E-07
 -6.7278465394338127E-06  4.4596313841793744E-07 -2.4755367200590683E-10
  6.6328821048955433E-06  2.8732655366070503E-08  1.5816522136447020E-06
 -8.9828821364790201E-04 -1.0359136126680824E-07  2.8732655366070503E-08
  1.3829509043716826E-05  9.0345303760841489E-04 -5.9563441027421504E-05
 -6.7278465394338127E-06  1.5816522136447020E-06  9.0345303760841489E-04
  5.9390719980001450E-02 -3.8860931526869535E-03  4.4596313841793744E-07
 -8.9828821364790201E-04 -5.9563441027421504E-05 -3.8860931526869535E-03
  0.1251238286495209    
 -1.7263018526136875E-03   1.039703369140625      0.8659646511077881    
  0.2599024176597595       2.128120422363281    
  1.5148657328545312E-10 -7.3402152411805588E-11 -1.4714315987873761E-08
 -6.3192055677063763E-07 -3.4522088299127063E-08 -7.3402152411805588E-11
  1.5436929743373184E-06 -5.5447091362736955E-10 -8.1613094948806975E-08
 -1.5131152758840472E-04 -1.4714315987873761E-08 -5.5447091362736955E-10
  1.5367089645224041E-06  6.8635607021860778E-05  4.2090109673154075E-06
 -6.3192055677063763E-07 -8.1613094948806975E-08  6.8635607021860778E-05
  3.2065853010863066E-03  1.9913408323191106E-04 -3.4522088299127063E-08
 -1.5131152758840472E-04  4.2090109673154075E-06  1.9913408323191106E-04
  1.7373077571392059E-02
  1.2108741793781519E-03   1.282915115356445      0.8532057404518127    
  8.5045360028743744E-03   1.965600013732910    
  3.6512477069594595E-11  8.9357354848829118E-10 -3.3482463468459400E-09
 -8.1875484170268464E-08  9.6036401053822829E-10  8.9357354848829118E-10
  3.0787202831561444E-06 -2.2171841251861224E-08 -2.7003440550288360E-07
 -1.5695679758209735E-04 -3.3482463468459400E-09 -2.2171841251861224E-08
  5.5774097518224153E-07  1.3075616152491421E-05 -4.9851792027766351E-07
 -8.1875484170268464E-08 -2.7003440550288360E-07  1.3075616152491421E-05
  3.5224124439992011E-04 -1.4417236343433615E-05  9.6036401053822829E-10
 -1.5695679758209735E-04 -4.9851792027766351E-07 -1.4417236343433615E-05
  1.7541546374559402E-02
 -7.3608336970210075E-04   1.297574043273926      0.8316786885261536    
  -1.011060714721680      -2.867138862609863    
  2.0176718074083055E-09  9.1418789205377493E-10 -2.5551665316925209E-07
 -1.5318933947128244E-05 -3.4175937457803229E-07  9.1418789205377493E-10
  7.4829795266850851E-06 -1.1038221003900617E-07 -6.1672653828281909E-06
 -9.3757675494998693E-04 -2.5551665316925209E-07 -1.1038221003900617E-07
  3.2483072573086247E-05  1.9545238465070724E-03  4.3123862269567326E-05
 -1.5318933947128244E-05 -6.1672653828281909E-06  1.9545238465070724E-03
  0.1181144416332245      2.5763250887393951E-03 -3.4175937457803229E-07
 -9.3757675494998693E-04  4.3123862269567326E-05  2.5763250887393951E-03
  0.1227073818445206    
"""
tav4 :: String
tav4 = """PU_zpositions:  190 4.06972837448 2.44204807281 7.82136058807 -0.621172726154 -6.80061435699 -1.73116350174 -5.42739343643 -7.10662841797 -6.32562208176 -3.72315001488 1.66695046425 6.55822181702 -7.12538957596 -0.389555871487 -2.8334877491 3.09819436073 -5.65534687042 12.068236351 -1.79448211193 5.73383188248 1.68428444862 2.1804420948 8.66328144073 -12.8040647507 -1.1730145216 -3.57441878319 6.21948480606 -1.26211774349 -3.4871032238 -9.48501300812 -8.33902263641 -1.71619582176 -1.56027853489 1.49686825275 -1.69698286057 1.69038307667 5.10251283646 -2.57128977776 0.749759852886 -2.58463263512 -9.792719841 -8.84095287323 -0.131224393845 -1.56865620613 -5.81232976913 4.21827507019 -4.92665529251 -5.84215211868 -5.74135446548 3.38353490829 -3.13945651054 4.30185222626 -12.6121692657 1.54116880894 1.38944470882 -6.84423398972 2.88845825195 -4.16181087494 6.3093957901 -1.70226609707 3.62256598473 -1.38095474243 1.69552695751 -9.44017601013 2.82410240173 -2.21053552628 2.34878325462 -8.67048835754 1.25067412853 9.49777984619 8.16330623627 -0.870663702488 -4.79498910904 1.78941035271 -7.03154611588 1.68979644775 -0.484967201948 -4.18258905411 0.0788396298885 -4.69477128983 2.32463097572 -2.10498857498 -5.34199571609 3.32180857658 -5.39752531052 -2.84948658943 -2.68618583679 1.0778503418 0.443690419197 -3.29635429382 0.936188876629 -4.41851854324 -3.29131436348 2.12316703796 -10.6452322006 -14.0393047333 3.74121594429 -8.4497051239 -5.68886137009 8.31489753723 -4.49255418777 -7.92309999466 -7.26154613495 -2.43943715096 2.87128973007 -8.41958713531 -5.04697036743 -2.6269865036 -3.01578998566 5.666908741 4.7386713028 4.83959341049 -12.2599534988 6.80844593048 -7.59651374817 1.77152347565 -3.49425053596 4.14569759369 2.39712738991 0.695241510868 0.351206511259 -1.00542604923 -0.592145264149 8.05185890198 1.35937333107 -3.23685288429 1.82836604118 -1.08040130138 -4.06748771667 -1.22976350784 -5.24559354782 4.77764129639 -7.92655897141 6.87241268158 8.90295886993 -10.4462614059 5.51054620743 4.28739690781 -0.413518726826 -2.84266161919 -4.82323074341 -3.47484374046 -6.56179046631 -5.6174902916 2.68036007881 -4.87207984924 -3.47317409515 -1.94823920727 -11.0047950745 -6.04952716827 -12.1523780823 -0.171474739909 1.82068359852 -11.1572389603 -2.97859430313 -3.65392804146 1.67614769936 -4.62239599228 4.72258663177 -3.13622426987 -9.94389533997 -13.6851511002 1.98555517197 4.60026597977 -10.9611978531 -1.63044011593 8.50263690948 -9.76078033447 0.933302462101 6.68330335617 -2.94098043442 -8.59897899628 -0.908704698086 -5.6248884201 -9.19552707672 -6.67034435272 3.34288668633 -2.66896915436 -5.85388660431 -6.08788156509 -9.28157234192 -3.39719057083 -2.08446788788 3.61256814003 4.3055267334 -3.20882606506 -1.37032854557 6.3657708168 -7.99672412872 7.93814659119
0.104794 0.168646 -1.00377 0.0015033299569 0.0 0.0 0.0 0.00151841994375 0.0 0.0 0.0 5.21037006378
1.0
4
-0.450663641447 1.35035226203 1.18063795337 0.0660153061812 -1.23642665653 1.75648085587e-06 1.09397257919e-09 2.97465732046e-07 -4.61079963543e-07 -2.34128183507e-08 1.09397257919e-09 1.44003013247e-06 8.51150190329e-08 2.84054323174e-07 -1.69398772414e-05 2.97465732046e-07 8.51150190329e-08 3.07370719383e-05 -7.83819778007e-05 -3.34401283908e-06 -4.61079963543e-07 2.84054323174e-07 -7.83819778007e-05 0.000201181290322 2.55465602095e-06 -2.34128183507e-08 -1.69398772414e-05 -3.34401283908e-06 2.55465602095e-06 0.000202862953302
0.425837572723 -1.33656916571 -0.200853553729 0.186440212098 0.447653308602 4.94152754982e-06 -1.85234905192e-09 7.72127236814e-07 -1.24222322029e-06 -3.99287580777e-09 -1.85234905192e-09 1.2965380165e-06 1.72925282982e-08 4.64915046905e-07 -1.68021942955e-05 7.72127236814e-07 1.72925282982e-08 2.40928802668e-05 -7.24880374037e-05 -2.46094759859e-06 -1.24222322029e-06 4.64915046905e-07 -7.24880374037e-05 0.000219607783947 7.73318163283e-07 -3.99287580777e-09 -1.68021942955e-05 -2.46094759859e-06 7.73318163283e-07 0.000218317465624
0.292514034965 1.39665579944 -0.993975573833 0.141543926193 0.40182813437 6.18352771653e-07 2.45095588269e-09 2.34424803125e-07 -8.39045242174e-07 -8.35598825688e-08 2.45095588269e-09 8.66051891535e-07 4.61750582215e-10 -3.98558910319e-07 -1.4314922737e-05 2.34424803125e-07 4.61750582215e-10 2.8809732612e-05 -8.27241237857e-05 2.18257969209e-06 -8.39045242174e-07 -3.98558910319e-07 -8.27241237857e-05 0.000238582899328 4.61562564169e-07 -8.35598825688e-08 -1.4314922737e-05 2.18257969209e-06 4.61562564169e-07 0.000236972875427
-0.29652562498 1.37864870921 -1.02387889924 0.192178996941 0.0749212341852 1.4916007558e-06 5.99006577673e-09 7.09483913397e-07 -2.9230166092e-06 -3.92529898363e-07 5.99006577673e-09 7.67293840909e-07 5.73736658183e-09 2.58630308281e-07 -1.14107451736e-05 7.09483913397e-07 5.73736658183e-09 2.14707033592e-05 -6.15742173977e-05 -1.75877414677e-06 -2.9230166092e-06 2.58630308281e-07 -6.15742173977e-05 0.000178089467227 1.27894770685e-06 -3.92529898363e-07 -1.14107451736e-05 -1.75877414677e-06 1.27894770685e-06 0.000170099316165
"""

