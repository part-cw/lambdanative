#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; List structures used for testing csv-read and csv-write

(define csv:test_line_feed (list (list "\"Test Cell1\"" "\"Test\nCell2\"" "Test")
                                 (list "Cell3" "\"Test,\nCell4\"" "Test")
                                 (list "Cell5" "\"Test\n\nCell6\"" "Test")
                                 (list)
                                 (list "Cell7" "\"Test,\n\nCell8\"" "Test")
                                 (list)
                                 (list "Cell9" "\"Test\n,\nCell10\"" "Test")
                                 (list "")
                                 (list "Cell11")))

(define csv:test_trend_list (list (list "1351654722.692798" " 121031-093842")
                                  (list "PhoneOxR2" "2012-09-18 23:41:48" "2580304613877d9f41559f8b1b54d06df9634c7c" "iphone")
                                  (list "Time" "HR" "SP" "SPBB" "ABC" "DEF" "GHI" "JKL" "MNO" "PQ" "RST" "BT" "Event")
                                  (list "1" "123" "98" "98" "131" "" "" "62.06607" "0" "0.40983" "56" "100" "Artifacts: Check connection:Oximeter")
                                  (list "2" "123" "98" "98" "131" "" "" "62.06607" "0" "0.40983" "56" "100")
                                  (list "3" "123" "98" "98" "129" "" "" "62.06607" "0.50505" "0.40983" "56" "100")
                                  (list "4" "123" "98" "98" "131" "" "" "62.06607" "0.50505" "0.40650" "56" "100")
                                  (list "4" "123" "98" "98" "129" "" "" "62.06607" "0.50505" "0.40650" "56" "100")))

;; Strings used for unit tests of csv-read and csv-write

;; Carriage returns are turned into line feeds (the new line used in csv-write) and there is always a comma at the end of a row (unless the row is completely empty)
(define csv:test_carriage_return_input "\"Test\015Cell12\",Test\015Cell13,\"Test,\015Cell14\",Test,\015Cell15,\"Test\015\015Cell16\",Test\015\015Cell17,\"Test,\015\015Cell18\",Test,\015\015Cell19,\"Test\015,\015Cell20\",Test\015,\015Cell21,\"")
(define csv:test_carriage_return_output "\"Test\nCell12\",Test,\nCell13,\"Test,\nCell14\",Test,\nCell15,\"Test\n\nCell16\",Test,\n\nCell17,\"Test,\n\nCell18\",Test,\n\nCell19,\"Test\n,\nCell20\",Test,\n,\nCell21,\",")

(define csv:test_trend_file1 "1337345694.423719, 120518-155454,
PhoneOxR,2012-04-11 03:58:46,4775e2bdcd8d3d62999e0f68316ea93fb833f2af,iphone,
User:,Annet,Site:,MUST,Comment:,,
Time,HR,SP,SPBB,Status,ABC,DEF,GHI,JKL,BT,Event,
1,75,98,98,129,1,,,97,94,
2,75,98,98,131,1,,,97,94,
3,75,98,97,129,1,,,97,94,
4,74,98,97,131,1,,,97,89,
5,75,98,98,129,1,,,97,89,
6,76,98,98,131,1,,,97,89,
7,75,98,98,131,1,,,97,89,
8,75,98,98,131,1,,,97,89,
9,73,98,98,131,1,,,97,89,
10,72,98,98,129,1,,,97,89,
11,71,98,97,131,1,,,97,89,
12,70,98,98,129,1,,,97,89,
13,69,98,97,131,1,,,97,89,
14,69,98,97,131,1,,,98,89,
15,74,98,98,129,1,,,98,89,
16,76,98,98,131,1,,,98,89,
17,81,98,98,131,1,,,98,89,
18,84,98,98,131,1,,,98,89,
19,83,98,98,129,1,,,98,89,
20,80,98,98,129,1,,,98,89,
21,78,98,98,129,1,,,98,89,
22,76,98,98,131,1,,,98,89,
23,74,98,98,131,1,,,98,89,
25,75,98,98,131,1,,,98,89,
26,75,98,98,131,1,,,98,89,
27,75,98,98,129,1,,,98,89,
28,75,98,98,129,1,,,98,89,
29,74,98,98,131,1,,,98,89,
30,74,98,98,129,1,,,98,89,
31,73,98,98,131,1,,,98,89,
32,73,98,98,129,1,,,98,89,
33,72,98,98,129,1,,,98,89,
34,72,98,98,131,1,,,97,89,
35,73,98,98,129,1,,,97,89,
36,74,98,98,131,1,,,97,89,
37,73,98,98,131,1,,,97,89,
38,74,98,98,131,1,,,97,89,
39,75,98,98,129,1,,,97,89,
40,75,98,98,131,1,,,97,89,
41,75,98,98,131,1,,,97,89,
42,76,98,98,129,1,,,97,89,
43,76,98,98,129,1,,,97,89,
44,75,98,98,129,1,,,97,89,
45,75,98,98,129,1,,,97,89,
46,75,98,98,131,1,,,97,89,
47,75,98,98,131,1,,,97,89,
48,75,98,98,129,1,,,97,89,
49,75,98,98,129,1,,,97,89,
51,75,98,98,131,1,,,98,89,
52,74,98,98,129,1,,,98,89,
53,74,98,98,129,1,,,98,89,
54,74,98,98,129,1,,,98,89,
55,74,98,98,129,0,,,98,89,
56,74,98,98,129,0,,,98,89,
57,74,98,98,129,0,,,98,89,
58,74,98,98,129,0,,,98,89,
59,74,98,98,131,0,,,98,89,
60,72,98,98,129,0,,,98,89,
61,72,98,98,129,0,,,98,89,
61,72,98,98,129,0,,,98,89,
Median:,75,,98.0,")

(define csv:test_trend_file2 "1351654722.692798, 121031-093842
PhoneOxR2,2012-09-18 23:41:48,2580304613877d9f41559f8b1b54d06df9634c7c,iphone
Time,HR,SP,SPBB,ABC,DEF,GHI,JKL,MNO,PQ,RST,BT,Event
1,123,98,98,131,,,62.06607,0,0.40983,56,100,Artifacts: Check connection:Oximeter
2,123,98,98,131,,,62.06607,0,0.40983,56,100
3,123,98,98,129,,,62.06607,0.50505,0.40983,56,100
4,123,98,98,131,,,62.06607,0.50505,0.40650,56,100
4,123,98,98,129,,,62.06607,0.50505,0.40650,56,100")



;; eof