{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AOC.Day9 where

import Control.Applicative (optional)
import Control.Arrow
import Control.Monad
import Control.Monad.State as State
import Data.Array as A
import Data.Foldable
import Data.Graph as Graph
import Data.List as L
import Data.List.Split
import Data.Map as M
import Data.Maybe
import Data.Sequence as Seq
import qualified Data.Text as T
import Data.Tree as Tree
import Data.Void (Void)
import Numeric.Natural (Natural)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.RawString.QQ

pairSums :: [Int] -> [Int]
pairSums = \case
  [] -> []
  [_] -> []
  x : xs -> ((+ x) <$> xs) ++ pairSums xs

solveA :: Int -> String -> Int
solveA preambleSize i =
  let xs :: [Int]
      xs = fmap read . lines $ i
      (seed, nums) = L.splitAt preambleSize xs
      go :: [Int] -> State.State (Seq.Seq Int) Int
      go (x : xs) = do
        preamble <- get
        let ps = pairSums . Data.Foldable.toList $ preamble
            (_ :<| newPreamble) = (preamble :|> x)
        put newPreamble
        if x `elem` ps
          then go xs
          else pure x
   in evalState (go nums) (Seq.fromList seed)

solutionA = solveA 25 input

ranges :: [Int] -> [[Int]]
ranges = L.filter (not . L.null) . L.tails <=< L.inits

rangeSums :: [Int] -> [(Int, Int, Int)]
rangeSums = fmap (\xs -> (L.sum xs, L.minimum xs, L.maximum xs)) . ranges

solutionB =
  (\(_, l, h) -> l + h)
    . head
    . L.filter (\(x, _, _) -> x == solutionA)
    . rangeSums
    . fmap read
    . lines
    $ input

sample :: String
sample =
  [r|35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576|]

input :: String
input =
  [r|16
19
41
7
20
3
45
40
37
25
5
22
43
48
4
23
18
47
28
11
10
42
35
6
34
21
8
9
12
7
13
38
14
15
16
17
20
19
60
22
24
27
46
44
28
18
70
23
42
25
26
21
29
30
91
34
31
32
33
35
43
41
45
47
65
39
48
49
46
44
50
51
52
63
53
54
89
59
61
64
66
105
87
68
74
92
93
124
83
154
97
217
176
120
94
122
111
106
107
246
127
123
173
125
199
142
210
151
293
185
175
207
177
299
201
383
232
200
219
450
394
213
229
234
296
401
372
267
324
530
317
328
854
352
730
518
586
377
414
560
932
432
541
453
759
447
442
463
619
749
584
591
729
641
1049
645
947
766
784
994
877
1307
824
846
1661
874
1094
1374
889
905
1689
1026
1340
1175
1225
1229
1375
2049
2532
1411
2064
1640
1550
1608
1670
2196
1713
2221
2279
2255
2845
1794
1915
1931
4328
2201
2366
2400
2404
3051
2899
3045
3402
3968
2961
3158
3190
3220
5333
6241
3507
3914
5120
3709
4186
4194
3725
3846
7760
4567
5421
7524
8645
5365
6363
11492
6006
6697
6378
6119
6348
8641
11438
7216
10087
7232
7434
7555
10686
13797
9211
7571
11799
17642
13351
15861
11371
11484
11713
12125
12354
12384
23283
12467
15559
13564
14448
21336
21571
24509
14666
23755
15126
19284
25064
27358
18942
22855
28026
25918
23084
43793
24097
23838
46367
24738
24851
26915
26031
50015
28012
29114
52869
29792
47310
47935
38226
34068
44006
41797
42026
42780
46693
46922
48576
47181
74607
48835
48689
52946
89732
54043
54927
63860
116729
57126
76295
68018
71589
81006
206461
82644
85803
83823
88978
113615
95757
166027
143775
95870
102732
97524
103762
103616
126516
108970
135933
165273
120986
125144
184622
162098
139607
152595
163650
166467
264830
169626
221014
198602
384664
204840
193394
199486
493634
324229
201140
207378
229956
278596
234114
414408
246130
294770
264751
292202
301705
476824
420500
330117
428392
368228
493372
391996
392880
464237
400772
394534
441492
408518
431096
575606
437334
464070
480244
498865
716113
771594
918316
556953
593907
820388
825630
1346708
762762
1012940
760224
823976
784876
839614
803052
795306
878826
1244544
845852
868430
901404
1826952
944314
979109
1055818
1150860
1739620
1317177
1957222
1378783
1586738
2882770
1522986
2903915
1545100
1555530
1580182
2737598
2424390
1671482
1641158
1714282
1747256
1769834
1812744
2218581
1923423
3078516
3350016
2206678
2468037
3969490
2695960
2901769
4142004
7319506
3100630
3103168
3186258
3125282
3135712
3221340
3932863
3953934
3312640
4714513
3461538
5321749
3582578
3736167
4825192
4902638
4674715
5108447
5668216
8793498
6214409
5799128
6002399
9260666
8668447
6203798
9822960
6260994
6346622
6357052
6533980
6774178
9463937
16238115
8485216
11117047
11208695
7318745
15015069
11021337
12427192
9783162
11467344
11670615
12737778
11801527
12002926
12263393
12464792
12550420
12703674
16169582
24740704
12880602
12891032
19693911
14092923
15803961
18268378
17101907
18340082
18527440
24097807
20804499
29896884
21250506
22674194
25441452
23472142
23804453
35735535
37205496
36695485
26983955
25254094
25584276
34697022
34331401
25771634
28694993
42331893
31194830
32905868
35370285
55135900
36867522
39331939
46772001
66315894
43924700
46504600
47928288
47276595
48726236
49388729
50838370
80792222
51025728
52755589
51355910
72237807
86103940
54466627
56966464
59889823
83372122
70526769
68276153
103738465
76199461
83256639
85836539
100683877
90429300
116162507
95893329
95204883
98114965
100227099
100414457
123263535
108322374
102381638
104111499
200641556
168503252
111433091
114356450
171210646
138802922
144475614
170941226
151532792
159456100
162036000
169093178
176265839
185634183
250236013
191098212
196307786
195431982
266147499
248587113
202796095
267778474
206493137
322474018
215544590
225789541
253159372
283278536
303931714
340783400
290335714
375000690
474376748
310988892
468912719
388430278
453032108
372573625
376732395
518014487
386530194
398228077
401925119
472640636
418340685
409289232
520937846
459652509
478948913
536778433
865479107
678932404
587210250
594267428
715784090
466456641
1108148096
1105224737
1325131616
846182703
749306020
945405554
770801702
763262589
816568762
2040915706
800153196
946067665
827629917
868941741
875745873
926109150
1353347195
1003235074
1053666891
1442194993
1924716858
1060724069
1182240731
1646547575
1215762661
1520107722
1512568609
1534064291
1549459216
1773697582
1853820087
1696910852
1563415785
1692314635
1627783113
2455413030
1753739067
1744687614
1801855023
1878980947
1929344224
2185475805
2114390960
2242964800
4200100644
2276486730
4155467677
2398003392
2735870383
3330153621
3806705595
3204883244
5606640351
3241773851
4152323882
3191198898
3255730420
3308103399
3320097748
3372470727
4142691006
4993053921
3808325171
5718101140
6079081591
4114820029
4299866765
5319274204
4519451530
4674490122
5012357113
5133873775
5589202290
6043973782
8389604195
6396082142
6432972749
8926738099
6446929318
10753571713
6499302297
9026204539
6628201147
6692568475
7180795898
8634271559
7923145200
8108191936
8819318295
8414686794
8789310151
8974356887
9193941652
9531808643
9686847235
10146230888
10723076065
11633176072
15237915434
19556321272
12829054891
12932275046
13680098195
15236239469
15133573856
20409923300
15107255269
25313274267
14800760411
15103941098
16031337136
16337831994
17203996945
17793675182
17389043681
17763667038
18168298539
18725750295
26920852324
40551189701
20869306953
28837173017
27936310160
33698361844
25761329937
26509153086
31138592405
28480858606
29904701509
34593040626
46600840055
29908015680
30832097547
31135278234
31441773092
32369169130
33726875675
46226216698
35152710719
53878461014
35931965577
63603063353
52682182261
46630636890
47378460039
59071588394
52270483023
53697640097
54242188543
95003553971
54990011692
58385560115
59812717189
67067243811
86431784784
67373738669
113293460509
61967375781
95044836445
114061600086
66096044805
68879586394
71084676296
113697880701
82562602467
83310425616
94009096929
98901119913
99648943062
101076100136
106512671566
108687651789
107939828640
109232200235
200521768495
113375571807
118198277304
125908761994
128063420586
167172144941
136253325063
184386525752
130846962175
134975631199
137180721101
139964262690
270811224865
153647278763
165873028083
336775093558
177319522545
200725043198
198550062975
208336594851
207588771702
250556292908
216627480429
217172028875
222607772042
559382865600
249628896870
268027683276
253972182580
258910382761
265822593374
317283785235
329397025150
348300857541
272156352300
533850276650
293611541453
713030144363
330966801308
415501924953
449475874845
375869585520
424216252131
425508623726
415925366553
424760800577
433799509304
439235252471
439779800917
481518154803
521999865856
531066735061
512882565341
547583724033
524732976135
755183053439
754157825727
565767893753
1072316700168
1053066600917
624578342761
669481126973
706836386828
746468726261
1058377852065
800085837651
791794952073
840141618684
840686167130
1371047069022
1285224560788
1090500869888
1193393078198
921297955720
994400720144
1034882431197
1235249020726
1331414729589
1113351617786
1190346236514
1464719961445
1312236620014
1272604280581
1640772004781
1294059469734
1376317513801
1415949853234
1765214238893
1538263678334
1858463689716
1591880789724
1631936570757
2304861580129
2557828841369
1915698675864
2193902236301
2225228667711
2107752337930
2111644192234
2507853301307
2328941900931
3884170815108
2303697854300
2385955898367
2462950517095
2566663750315
2584840900595
2648921794382
3234781203517
4017892469124
2792267367035
3223817360481
3130144468058
5539642783646
3547635246621
3507579465588
3739688908687
4023451013794
4027342868098
4219396530164
4301654574231
6608291914389
5731670661788
6299846832623
4714897799298
4791892418026
6027048570552
5620737101884
4848906415462
5255217884130
5151504650910
5233762694977
10054391438650
5922411835093
9470047081714
6016084727516
6677779714679
6637723933646
7055214712209
7287324155308
10794903620896
16354238271273
8934294329462
9068302945626
8521051104395
11148753248085
9506790217324
17341715593958
12704828285231
9563804214760
11829284365589
10000411066372
10082669110439
17589354050021
10385267345887
11073916486003
11156174530070
18361240641311
11938496562609
12653808661162
12693864442195
13315503648325
15706026879272
18436077403393
16794114372632
26009368090520
30035580036153
18906318450282
18027841321719
18084855319155
20580706703327
19070594432084
28470122665042
19564215281132
19646473325199
27868030858635
38007713966510
20467936456326
34878969691787
21459183831890
23727725147165
31560127111444
54525443016986
24592305223771
43351083684478
51124342392576
29021530527597
58606694838952
34821955694351
35700432822914
45079962522604
49929306496932
36934159772001
37155449751239
58475650422836
38634809713216
38717067757283
41105657157089
60883174898404
40114409781525
41927120288216
44195661603491
45060241680097
45186908979055
56152432335215
48320030370936
53613835751368
59414260918122
78173039378829
82041530069741
79082570039455
63843486221948
70522388517265
74936365475876|]
