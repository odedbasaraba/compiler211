import os
import shutil

# change this to your local directory - the folder where all the .ml files are
directory = "compiler211"



path_tests_folder = "{}/compilerCases".format(os.getcwd())
if(os.path.isdir(path_tests_folder)):

    for filename in os.listdir(path_tests_folder):
        current_path = os.path.join(path_tests_folder,filename)
        try:
            if os.path.isfile(current_path) or os.path.islink(current_path):
                os.unlink(current_path)
            elif os.path.isdir(current_path):
                shutil.rmtree(current_path)
        except Exception as e:
                # nothing to be done...
                print("")
else:
    try:
        os.mkdir(path_tests_folder)
    except OSError:
        print ("Creation of the directory %s failed" % path_tests_folder)
         



all_tests = [
    {},
    ####CONSTS#####
    {"name":"test1", "folder":"1" , "input":"5" ,"expected":"5"},

    {"name":"test2", "folder":"2", "input":"#t" ,"expected":"#t"},

    {"name":"test3", "folder":"3", "input":"#\\g" ,"expected":"#\\g"},

    #{"name":"test3.5.scm", "folder":"3.5", "input":"5.5" ,"expected":"5.5"},

    {"name":"test4", "folder":"4", "input":"\"hello habibi supreme\"" ,"expected":"\"hello habibi supreme\"" },

    {"name":"test5", "folder":"5", "input":"'a" ,"expected":"a"},

    {"name":"test6", "folder":"6", "input":"'(1 2 3)" ,"expected":"(1 2 3)" },

    {"name":"test7", "folder":"7", "input":"(make-string 2)" ,"expected":"(1 2 #t)"},

    ####### Seq, If, and, or 
    {"name":"test8", "folder":"8", "input":"(begin 1 2 3 4 5)" ,"expected":"5"},

    {"name":"test9", "folder":"9", "input":"(if #t 5 4)" ,"expected":"5"},

    {"name":"test10", "folder":"10", "input":"(if #f 5 4)" ,"expected":"4"},

    {"name":"test11", "folder":"11", "input":"(and 1 2 3 4 5)" ,"expected":"5"},

    {"name":"test12", "folder":"12", "input":"(and 1 #f 3 4 5)" ,"expected":"#f"},

    {"name":"test13", "folder":"13", "input":"(or 1 2 #f 4 5)" ,"expected":"1"},

    {"name":"test14", "folder":"14", "input":"(or #f #f #f #f #f)" ,"expected":"#f"},

    {"name":"test15", "folder":"15", "input":"(begin (and 1 2) (or 3 4))" ,"expected":"3"},

    {"name":"test16", "folder":"16", "input":"(if (if #t #f #t) 4 '(1 2 3) )" ,"expected":"(1 2 3)"},

    #### Define, Set
    
    {"name":"test17", "folder":"17", "input":"(define x #t) x" ,"expected":"#t"},

    {"name":"test18", "folder":"18", "input":"(define x (if (and #f #f) 70 '())) x" ,"expected":"()"},

    {"name":"test19", "folder":"19", "input":"(define x (if (and #f #f) 70 '())) (set! x 5) x" ,"expected":"5"},

    #### Lambdas and applic
    {"name":"test20", "folder":"20", "input":"(define func (lambda () 5)) (func)" ,"expected":"5"},

    {"name":"test21", "folder":"21", "input":"((lambda () 5))" ,"expected":"5"},

    {"name":"test22", "folder":"22", "input":"((lambda () ((lambda () 5))))" ,"expected":"5"},

    {"name":"test23", "folder":"23", "input":"((lambda () ((lambda () ((lambda () \"hello habibi supreme\"))))))" ,"expected":"\"hello habibi supreme\""},
 
    {"name":"test24", "folder":"24", "input":"((lambda () (if 1 2 3) ))" ,"expected":"2"}, 

    {"name":"test25", "folder":"25", "input":"((lambda (x y z) (if x y z)) #t \"Nissim: HALAWA!\" 5)" ,"expected":"\"Nissim: HALAWA!\""},

    {"name":"test26", "folder":"26", "input":"(define func (lambda (x) x)) (func #\\$)" ,"expected":"#\\$"},

    {"name":"test27", "folder":"27", "input":"(define func (lambda () 5)) (define function123 (lambda (x y) y)) (function123 6 (func))" ,"expected":"5"},

    {"name":"test28", "folder":"28", "input":"(define func (lambda (x y) x)) (define function_pass (lambda (f x y) (f x y)))  (function_pass func #t #F)" ,"expected":"#t"},

    {"name":"test29", "folder":"29", "input":"(define weber (lambda () ((lambda () \"COOK!\")))) (define dira (lambda (fun) (fun))) (dira weber)" ,"expected":"\"COOK!\""},

    {"name":"test30", "folder":"30", "input":"(define x (lambda (x pred) (if pred x #f))) (define y (lambda () ((lambda () ((lambda () #t)))))) (begin 5 6 7 8 '(1 2 3) (x \"PASS\" (y)))" ,"expected":"\"PASS\""},

    {"name":"test31", "folder":"31", "input":"((lambda (x) ((lambda (y) (begin (set! x  7) x)) 0)) 5)" ,"expected":"7"},

    {"name":"test32", "folder":"32", "input":"(define                       tal (if #f 2 3 ))\n\ntal" ,"expected":"3"},

    {"name":"test33", "folder":"33", "input":"(define moshe (or #f 2 3 ))\n\n(define moshe 12)\nmoshe" ,"expected":"12"},

    {"name":"test34", "folder":"34", "input":"(define tall \"str\")\n\n(define all 1)\n\nall" ,"expected":"1"},


    {"name":"test35", "folder":"35", "input":"(define loAvi \"not avi\")\n(define avi 5)\n(define aaaaa 's)\n\n\nloAvi" ,"expected":"\"not avi\""},

    {"name":"test36", "folder":"36", "input":"(define a 5)\n\n(define b a)\n\nb" ,"expected":"5"},

    {"name":"test37", "folder":"37", "input":"(define abc 3)\n\n(or #f abc 2)" ,"expected":"3"},

    {"name":"test38", "folder":"38", "input":"(define abc #f)\n\n(define xxx '(1 2 3))\n\n(or #f abc xxx)" ,"expected":"(1 2 3)"},

    {"name":"test39", "folder":"39", "input":"(if #f  4   (if #f 3 8)) " ,"expected":"8"},

    {"name":"test40", "folder":"40", "input":"(or)" ,"expected":"#f"},

    {"name":"test41", "folder":"41", "input":"(and)" ,"expected":"#t"},

    {"name":"test42", "folder":"42", "input":"(or 1)" ,"expected":"1"},

    {"name":"test43", "folder":"43", "input":"(or #f (if #f 3 42) 3)" ,"expected":"42"},

    {"name":"test44", "folder":"44", "input":"(or #f 'moshe 3 4 \"banana\")" ,"expected":"moshe"},

    {"name":"test45", "folder":"45", "input":"(or (or #f (or 2 3) #f) 4)" ,"expected":"2"},

    {"name":"test46", "folder":"46", "input":"(and (or #f) 5 (or 1 2 3))" ,"expected":"#f"},

    {"name":"test47", "folder":"47", "input":"(define foo (lambda (x . y) (lambda (z) y))) ((foo 1)2)" ,"expected":"()"},

    {"name":"test48", "folder":"48", "input":"(((lambda (w . r) (lambda () w)) 1 2 3 4))" ,"expected":"1"},

    {"name":"test49", "folder":"49", "input":"(((lambda () (lambda (w . r) r)))1 2)" ,"expected":"(2)"},

    {"name":"test50", "folder":"50", "input":"((((lambda (x . y) (lambda (w . r) (lambda (e) r))) 2 9) 3 4 5)6) " ,"expected":"(4 5)"},

    {"name":"test51", "folder":"51", "input":"((((lambda () (lambda (x) (lambda () x))))2))" ,"expected":"2"},

    {"name":"test52", "folder":"52", "input":"((((lambda (x) (lambda () (lambda () x))) 2)))" ,"expected":"2"},
    
    {"name":"test53", "folder":"53", "input":"(((lambda () (lambda (x) 2)))1)" ,"expected":"2"},

    {"name":"test54", "folder":"54", "input":"(or 1)" ,"expected":"1"},

    {"name":"test55", "folder":"55", "input":"((((lambda (x) (lambda (y) (lambda (z) z)))2)3)4) " ,"expected":"4"},

    {"name":"test56", "folder":"56", "input":"((((lambda (x y . z) (lambda () (lambda () z))) 1 2 )))" ,"expected":"()"},

    {"name":"test57", "folder":"57", "input":"(((lambda (x y . z) (lambda () z)) 1 2 3 4 5 6))" ,"expected":"(3 4 5 6)"},

    {"name":"test58", "folder":"58", "input":"(((lambda (x y . z) (lambda () z)) 1 2 )) " ,"expected":"()"},

    {"name":"test59", "folder":"59", "input":"(define f (lambda (x y z) (lambda (w r) (lambda () (+ x r) z)))) (((f 1 2 3) 4 5))" ,"expected":"3"},

    
    
    {"name":"test60", "folder":"60", "input":"'#(2.4 (2 . 4))" ,"expected":"1"},

    {"name":"test61", "folder":"61", "input":"(if (zero? 0)	(or #f #t)	0)" ,"expected":"1"},
    
    {"name":"test62", "folder":"62", "input":"(define x (+ 2)) ((lambda (x) 	(set! x (+ 2 3))	x) x)" ,"expected":"1"},


    {"name":"test63", "folder":"63", "input":"(letrec ((loop (lambda (r) 				(if (= r 0)					0					(loop (- r 1))))))	(loop 220000))" ,"expected":"1"},

    {"name":"test64", "folder":"64", "input":"(eq? ((lambda (x . y) 	(cons x y)) 'a 'b 'c 'd)	'(a b c d))" ,"expected":"1"},

    {"name":"test65", "folder":"65", "input":"((lambda x     (map cdr x))   '(97 #\\a) (list 0 #\\nul) `(32.0 #\\space) (cons 10 #\\newline))" ,"expected":"1"},


    {"name":"test66", "folder":"66", "input":"(define a 'alpha) (define b 'beta) (define l ((lambda (x y)               (list                  (lambda () (set! x y))                 (lambda () (cons x y)))) a b)) ((car (cdr l))) ((car l)) ((car (cdr l)))" ,"expected":"1"},

    {"name":"test67", "folder":"67", "input":"(define a \"alpha\") (define b \"alpha\") (string-set! a 0 #\\b) (and (eq? a b) (equal? (string-ref a 0) (string-ref b 0)))" ,"expected":"1"},

    {"name":"test68", "folder":"68", "input":"(apply - 0 `(3 ,@(append '(4 4) '(4))))" ,"expected":"1"},

    {"name":"test69", "folder":"69", "input":"(define foo 	(lambda (n e)      		(if (= n (* 2 (/ n 2)))               			(foo (/ n 2) (+ e 1))                            			e)))                                          (apply foo '(64 0))" ,"expected":"1"},

    {"name":"test70", "folder":"70", "input":"(define rocket (char->integer #\\r)) (define frily (char->integer #\\f)) (not (> frily rocket))" ,"expected":"1"},

    {"name":"test71", "folder":"71", "input":"(define sfx     (lambda (bool)      (if bool           (lambda (v s i)            (vector-set! v i (string-ref s i))            v)          (lambda (v s i)            (string-set! s i (vector-ref v i))             s)))) (let ((vec (make-vector 5 #\\a)) 		(str (make-string 5 #\\b))) 	(cons ((sfx #t) vec str 3) ((sfx #f) vec str 2)))" ,"expected":"1"},

    {"name":"test72", "folder":"72", "input":"(let ((baf (lambda (f)                (lambda (n)                  (if (> n 0)                      `(* ,n ,((f f) (- n 1)))                      \"end\")))))     ((baf baf) 3))" ,"expected":"1"},

    {"name":"test73", "folder":"73", "input":"(define foo (lambda (x) 				(cons 					(begin (lambda () (set! x 1) 'void)) 					(lambda () x)))) (define p (foo 2)) (let* ((a ((cdr p)))    (b ((car p)))       (c ((cdr p))))    (cons a (cons b (cons c '()))))" ,"expected":"1"},

    {"name":"test74", "folder":"74", "input":"(((((lambda (x)     (lambda (y)       (lambda (z)         (lambda w           (append x y z w))))) '(a)) '(b)) '(c)) 'd 'e)" ,"expected":"1"},

    {"name":"test75", "folder":"75", "input":"(define foo (lambda (x) 	(let ((x (+ x 1))) 		(lambda (y) 			(let ((y (+ y 1))) 				(lambda (z)  					(+ x y z))))))) 				(define f1 (foo 1)) (define f12 (f1 2)) (f12 3)" ,"expected":"1"},

    {"name":"test76", "folder":"76", "input":"(let ((x 1)) 	(eq? (if #f #f) (set! x 2)))" ,"expected":"1"},

    {"name":"test77", "folder":"77", "input":"(if (eq? '() (list)) 'OK 'NOK)" ,"expected":"1"}, 


    {"name":"test78", "folder":"78", "input":"(map map (list              (lambda (x) (+ x x))              (lambda (x) (- x))             (lambda (x) (* x x))              (lambda (x)  (/ x)))       '((1 2.0) (3 4e0) (5 6e-1) (7 8e1)))" ,"expected":"1"},

    {"name":"test79", "folder":"79", "input":"(define length 	(lambda (l)		(if (null? l) 0			(+ 1 (length (cdr l)))))) (= (length ''''''''a) (length ``````````a))" ,"expected":"1"},
    
    {"name":"test80", "folder":"80", "input":"(define str (make-string 5 #\\space)) (string-set! str 0 #\\t) (string-set! str 1 #\\tab) (string-set! str 3 #\\n) (string-set! str 4 #\\newline) str" ,"expected":"1"},

    {"name":"test81", "folder":"81", "input":"(define ascii 	(lambda ()		(letrec ((loop (lambda (i)						(if (< i 127)							(cons `(,i ,(integer->char i)) (loop (+ 1 i)))							'()))))			(loop (char->integer #\\space))))) (ascii)" ,"expected":"1"},

    {"name":"test82", "folder":"82", "input":"(define list? (lambda (x) 	(or (null? x) (and (pair? x) (list? (cdr x)))))) (define describe   (lambda (e)     (cond      ((list? e) `(list ,@(map describe e)))      ((pair? e) `(cons ,(describe (car e))         ,(describe (cdr e))))      ((or (null? e) (symbol? e)) `',e)      (else e))))      (describe '(sym \"str\" #\\c 1))" ,"expected":"1"},

    {"name":"test83", "folder":"83", "input":"(define Y 	(lambda (foo)	((lambda (f) (foo (lambda (x) ((f f) x))))		(lambda (f) (foo (lambda (x) ((f f) x))))))) (define max	(Y (lambda (f)		(lambda (l) 			(if (null? l) 0				(let ((cdr-max (f (cdr l))))					(if (> (car l) cdr-max) (car l) cdr-max))))))) (define depth	(lambda (f)		(lambda (t)			(if (pair? t)				(+ 1 (max (map f t)))				0)))) ((Y depth) '((1 2 3) (4 (5 (6))) (((((7 (8) 9 10 11 12) 13) 14)))))" ,"expected":"1"},

    {"name":"test84", "folder":"84", "input":"((lambda (a1 a2 a3 a4 a5)      ((lambda (b1 b2 b3 b4)         ((lambda (c1 c2 c3)            ((lambda (d1 d2)               ((lambda (e1)                   e1) d1)) c1 c2)) b1 b2 b3)) a1 a2 a3 a4))    1 2 3 4 5)" ,"expected":"1"},

    {"name":"test85", "folder":"85", "input":"(((lambda (x) 	(lambda (y . z) 		(if `notbool (+ x  (* y (car z)) (car (cdr z)))))) 9) 10 11 12)" ,"expected":"1"},

    {"name":"test86", "folder":"86", "input":"(if '2/3 (symbol? '2/3) '0notasymbol)" ,"expected":"1"},

    {"name":"test87", "folder":"87", "input":"((lambda (x . y)      ((lambda ()         (set-cdr! x y)))     x)   (cons 1 2))" ,"expected":"1"},

    {"name":"test88", "folder":"88", "input":"(map (lambda (x) (apply x '(()))) ((lambda ()	`(,(lambda (a) 1)		,(lambda (b) 2)		,(lambda (c) 3)		,(lambda (d) 4)		,(lambda (e) 5)		,(lambda (f) 6)		,(lambda (g) 7)		,(lambda (h) 8)		,(lambda (i) 9)		,(lambda (j) 10)		,(lambda (k) 11)		,(lambda (l) 12)		,(lambda (m) 13)		,(lambda (n) 14)		,(lambda (o) 15)		,(lambda (p) 16)		,(lambda (q) 17)		,(lambda (r) 18)		,(lambda (s) 19)		,(lambda (t) 20)		,(lambda (u) 21)		,(lambda (v) 22)		,(lambda (w) 23)		,(lambda (x) 24)		,(lambda (y) 25)		,(lambda (z) 26)))))" ,"expected":"1"},
    
    {"name":"test89", "folder":"89", "input":"(car '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380 381 382 383 384 385 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 418 419 420 421 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 478 479 480 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 496 497 498 499 500 501 502 503 504 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 531 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549 550 551 552 553 554 555 556 557 558 559 560 561 562 563 564 565 566 567 568 569 570 571 572 573 574 575 576 577 578 579 580 581 582 583 584 585 586 587 588 589 590 591 592 593 594 595 596 597 598 599 600 601 602 603 604 605 606 607 608 609 610 611 612 613 614 615 616 617 618 619 620 621 622 623 624 625 626 627 628 629 630 631 632 633 634 635 636 637 638 639 640 641 642 643 644 645 646 647 648 649 650 651 652 653 654 655 656 657 658 659 660 661 662 663 664 665 666 667 668 669 670 671 672 673 674 675 676 677 678 679 680 681 682 683 684 685 686 687 688 689 690 691 692 693 694 695 696 697 698 699 700 701 702 703 704 705 706 707 708 709 710 711 712 713 714 715 716 717 718 719 720 721 722 723 724 725 726 727 728 729 730 731 732 733 734 735 736 737 738 739 740 741 742 743 744 745 746 747 748 749 750 751 752 753 754 755 756 757 758 759 760 761 762 763 764 765 766 767 768 769 770 771 772 773 774 775 776 777 778 779 780 781 782 783 784 785 786 787 788 789 790 791 792 793 794 795 796 797 798 799 800 801 802 803 804 805 806 807 808 809 810 811 812 813 814 815 816 817 818 819 820 821 822 823 824 825 826 827 828 829 830 831 832 833 834 835 836 837 838 839 840 841 842 843 844 845 846 847 848 849 850 851 852 853 854 855 856 857 858 859 860 861 862 863 864 865 866 867 868 869 870 871 872 873 874 875 876 877 878 879 880 881 882 883 884 885 886 887 888 889 890 891 892 893 894 895 896 897 898 899 900 901 902 903 904 905 906 907 908 909 910 911 912 913 914 915 916 917 918 919 920 921 922 923 924 925 926 927 928 929 930 931 932 933 934 935 936 937 938 939 940 941 942 943 944 945 946 947 948 949 950 951 952 953 954 955 956 957 958 959 960 961 962 963 964 965 966 967 968 969 970 971 972 973 974 975 976 977 978 979 980 981 982 983 984 985 986 987 988 989 990 991 992 993 994 995 996 997 998 999 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 1023 1024 1025 1026 1027 1028 1029 1030 1031 1032 1033 1034 1035 1036 1037 1038 1039 1040 1041 1042 1043 1044 1045 1046 1047 1048 1049 1050 1051 1052 1053 1054 1055 1056 1057 1058 1059 1060 1061 1062 1063 1064 1065 1066 1067 1068 1069 1070 1071 1072 1073 1074 1075 1076 1077 1078 1079 1080 1081 1082 1083 1084 1085 1086 1087 1088 1089 1090 1091 1092 1093 1094 1095 1096 1097 1098 1099 1100 1101 1102 1103 1104 1105 1106 1107 1108 1109 1110 1111 1112 1113 1114 1115 1116 1117 1118 1119 1120 1121 1122 1123 1124 1125 1126 1127 1128 1129 1130 1131 1132 1133 1134 1135 1136 1137 1138 1139 1140 1141 1142 1143 1144 1145 1146 1147 1148 1149 1150 1151 1152 1153 1154 1155 1156 1157 1158 1159 1160 1161 1162 1163 1164 1165 1166 1167 1168 1169 1170 1171 1172 1173 1174 1175 1176 1177 1178 1179 1180 1181 1182 1183 1184 1185 1186 1187 1188 1189 1190 1191 1192 1193 1194 1195 1196 1197 1198 1199 1200 1201 1202 1203 1204 1205 1206 1207 1208 1209 1210 1211 1212 1213 1214 1215 1216 1217 1218 1219 1220 1221 1222 1223 1224 1225 1226 1227 1228 1229 1230 1231 1232 1233 1234 1235 1236 1237 1238 1239 1240 1241 1242 1243 1244 1245 1246 1247 1248 1249 1250 1251 1252 1253 1254 1255 1256 1257 1258 1259 1260 1261 1262 1263 1264 1265 1266 1267 1268 1269 1270 1271 1272 1273 1274 1275 1276 1277 1278 1279 1280 1281 1282 1283 1284 1285 1286 1287 1288 1289 1290 1291 1292 1293 1294 1295 1296 1297 1298 1299 1300 1301 1302 1303 1304 1305 1306 1307 1308 1309 1310 1311 1312 1313 1314 1315 1316 1317 1318 1319 1320 1321 1322 1323 1324 1325 1326 1327 1328 1329 1330 1331 1332 1333 1334 1335 1336 1337 1338 1339 1340 1341 1342 1343 1344 1345 1346 1347 1348 1349 1350 1351 1352 1353 1354 1355 1356 1357 1358 1359 1360 1361 1362 1363 1364 1365 1366 1367 1368 1369 1370 1371 1372 1373 1374 1375 1376 1377 1378 1379 1380 1381 1382 1383 1384 1385 1386 1387 1388 1389 1390 1391 1392 1393 1394 1395 1396 1397 1398 1399 1400 1401 1402 1403 1404 1405 1406 1407 1408 1409 1410 1411 1412 1413 1414 1415 1416 1417 1418 1419 1420 1421 1422 1423 1424 1425 1426 1427 1428 1429 1430 1431 1432 1433 1434 1435 1436 1437 1438 1439 1440 1441 1442 1443 1444 1445 1446 1447 1448 1449 1450 1451 1452 1453 1454 1455 1456 1457 1458 1459 1460 1461 1462 1463 1464 1465 1466 1467 1468 1469 1470 1471 1472 1473 1474 1475 1476 1477 1478 1479 1480 1481 1482 1483 1484 1485 1486 1487 1488 1489 1490 1491 1492 1493 1494 1495 1496 1497 1498 1499 1500 1501 1502 1503 1504 1505 1506 1507 1508 1509 1510 1511 1512 1513 1514 1515 1516 1517 1518 1519 1520 1521 1522 1523 1524 1525 1526 1527 1528 1529 1530 1531 1532 1533 1534 1535 1536 1537 1538 1539 1540 1541 1542 1543 1544 1545 1546 1547 1548 1549 1550 1551 1552 1553 1554 1555 1556 1557 1558 1559 1560 1561 1562 1563 1564 1565 1566 1567 1568 1569 1570 1571 1572 1573 1574 1575 1576 1577 1578 1579 1580 1581 1582 1583 1584 1585 1586 1587 1588 1589 1590 1591 1592 1593 1594 1595 1596 1597 1598 1599 1600 1601 1602 1603 1604 1605 1606 1607 1608 1609 1610 1611 1612 1613 1614 1615 1616 1617 1618 1619 1620 1621 1622 1623 1624 1625 1626 1627 1628 1629 1630 1631 1632 1633 1634 1635 1636 1637 1638 1639 1640 1641 1642 1643 1644 1645 1646 1647 1648 1649 1650 1651 1652 1653 1654 1655 1656 1657 1658 1659 1660 1661 1662 1663 1664 1665 1666 1667 1668 1669 1670 1671 1672 1673 1674 1675 1676 1677 1678 1679 1680 1681 1682 1683 1684 1685 1686 1687 1688 1689 1690 1691 1692 1693 1694 1695 1696 1697 1698 1699 1700 1701 1702 1703 1704 1705))" ,"expected":"1"},

    {"name":"test90", "folder":"90", "input":"((lambda () ((lambda (a b c d e) e) 'a 'b 'c 'd 'e)))" ,"expected":"1"},

    {"name":"test91", "folder":"91", "input":"'#(#\\v #(#\\e #(#\\c #(#\\t #(#\\o #(#\\r #(#\\s #(#\\o #\\f) #\\v) #\\e) #\\c) #\\t) #\\o) #\\r) #\\s)" ,"expected":"1"},

    {"name":"test92", "folder":"92", "input":"(/ (+ 0.2 (* 2 (- 0.6667 0.2667))) (/ 1.0 2))" ,"expected":"1"},

    {"name":"test93", "folder":"93", "input":"(and (integer? (- 7.5 (* 3.75 2))) (/ 1 0))" ,"expected":"1"},

    {"name":"test94", "folder":"94", "input":"((lambda x    ((lambda (_) '())     (set-car! x 'd))    x) 'a 'b 'c)" ,"expected":"1"},

    {"name":"test95", "folder":"95", "input":"((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) ((lambda s `(left ,@s right)) 1 2 3))))))))))))))))" ,"expected":"1"},

    {"name":"test96", "folder":"96", "input":"((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s ((lambda s s) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) s)) 'mary 'had 'a 'little 'lambda)" ,"expected":"1"},

    {"name":"test97", "folder":"97", "input":"((lambda (x x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99 x100) (x x x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99 x100))  (lambda (x x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99 x100) (x x x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99 x100))     1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100)" ,"expected":"1"},

    {"name":"test98", "folder":"98", "input":"(((((lambda (a)       (lambda (b)         (((lambda (a) (lambda (b) ((a b) (lambda (x) (lambda (y) y))))) 	  ((lambda (n) 	     ((n (lambda (x) (lambda (x) (lambda (y) y)))) 	      (lambda (x) (lambda (y) x)))) 	   (((lambda (a) 	       (lambda (b) 		 ((b (lambda (n) 		       ((lambda (p) (p (lambda (a) (lambda (b) b)))) 			((n (lambda (p) 			      (((lambda (a) 				  (lambda (b) (lambda (c) ((c a) b)))) 				((lambda (n) 				   (lambda (s) 				     (lambda (z) (s ((n s) z))))) 				 ((lambda (p) 				    (p (lambda (a) (lambda (b) a)))) 				  p))) 			       ((lambda (p) 				  (p (lambda (a) (lambda (b) a)))) 				p)))) 			 (((lambda (a) 			     (lambda (b) (lambda (c) ((c a) b)))) 			   (lambda (x) (lambda (y) y))) 			  (lambda (x) (lambda (y) y))))))) 		  a))) 	     a) 	    b))) 	 ((lambda (n) 	    ((n (lambda (x) (lambda (x) (lambda (y) y)))) 	     (lambda (x) (lambda (y) x)))) 	  (((lambda (a) 	      (lambda (b) 		((b (lambda (n) 		      ((lambda (p) (p (lambda (a) (lambda (b) b)))) 		       ((n (lambda (p) 			     (((lambda (a)  				 (lambda (b) (lambda (c) ((c a) b)))) 			       ((lambda (n) 				  (lambda (s)  				    (lambda (z) (s ((n s) z))))) 				((lambda (p) 				   (p (lambda (a) (lambda (b) a)))) 				 p))) 			      ((lambda (p) 				 (p (lambda (a) (lambda (b) a)))) 			       p)))) 			(((lambda (a) 			    (lambda (b) (lambda (c) ((c a) b)))) 			  (lambda (x) (lambda (y) y))) 			 (lambda (x) (lambda (y) y))))))) 		 a))) 	    b) 	   a)))))     ((lambda (n)        ((lambda (p) (p (lambda (a) (lambda (b) b)))) 	((n (lambda (p) 	      (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) 		((lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))) 		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p))) 	       (((lambda (a) 		   (lambda (b) 		     ((b (a (lambda (a) 			      (lambda (b) 				((a (lambda (n) 				      (lambda (s) 					(lambda (z) (s ((n s) z)))))) 				 b))))) 		      (lambda (x) (lambda (y) y))))) 		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p)) 		((lambda (p) (p (lambda (a) (lambda (b) b)))) p))))) 	 (((lambda (a) (lambda (b) (lambda (c) ((c a) b)))) 	   (lambda (x) x)) 	  (lambda (x) x)))))      (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))    (((lambda (a)        (lambda (b) 	 ((b (a (lambda (a) 		  (lambda (b) 		    ((a (lambda (n) 			  (lambda (s) (lambda (z) (s ((n s) z)))))) 		     b))))) 	  (lambda (x) (lambda (y) y)))))      (((lambda (a) 	 (lambda (b) 	   ((b (a (lambda (a) 		    (lambda (b) 		      ((a (lambda (n) 			    (lambda (s) (lambda (z) (s ((n s) z)))))) 		       b))))) 	    (lambda (x) (lambda (y) y)))))        ((lambda (x) (lambda (y) (x (x (x y))))) 	(lambda (x) (lambda (y) (x (x y))))))       (lambda (x) (lambda (y) (x (x (x y)))))))     (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))   #t)  #f)" ,"expected":"1"}, 

    {"name":"test99", "folder":"99", "input":"((lambda (x) (x x 10000)) (lambda (x n)    (if (zero? n) #t        (x x (- n 1)))))" ,"expected":"1"},

    {"name":"test100", "folder":"100", "input":"(define fact (lambda (n)     (if (= 0 n)         1         (* n (fact (- n 1)))))) (fact 5)" ,"expected":"1"},

    {"name":"test101", "folder":"101", "input":"(define foo     (lambda (x)        (lambda (y)            (lambda (z)                (if x y z))))) (((foo #t) 1) 'moshe)" ,"expected":"1"},

    {"name":"test102", "folder":"102", "input":"(define foo     (lambda (x)        (lambda (y)            (+ x y)))) ((foo 1) 2)" ,"expected":"1"},

    {"name":"test103", "folder":"103", "input":"(cons 1 '( 2 3))" ,"expected":"1"},


    {"name":"test104", "folder":"104", "input":"(define foo     (lambda (x y z . w)         (cons x (cons y (cons z w))))) (foo 1 2 3 4 5)" ,"expected":"1"}, 

    {"name":"test105", "folder":"105", "input":"(define foo     (lambda (x y z . w)         w)) (foo 1 2 3 4 5)" ,"expected":"1"},


    {"name":"test106", "folder":"106", "input":"(((lambda (a b . c) (lambda (x) (cons x c))) 1 2 3 4 5 6 7) 8)" ,"expected":"1"},


    {"name":"test107", "folder":"107", "input":"(((lambda (a b) (lambda (x) b)) 1 2) 8)" ,"expected":"1"},

    {"name":"test108", "folder":"108", "input":"(define foo       (lambda (x y z . w)           (lambda vardic              (cons  (cons x (cons y (cons z '()))) (cons w vardic))))) ((foo 1 2 3 4 5))" ,"expected":"1"},


    {"name":"test109", "folder":"109", "input":"(define foo       (lambda (x y z . w)           (lambda vardic              (cons  (cons x (cons y (cons z '()))) (cons w vardic))))) ((foo 1 2 3 4 5) 6 7 8 9 10 11 12 13 14 15 'moshe)" ,"expected":"1"},


    {"name":"test110", "folder":"110", "input":"(define foo       (lambda (x y z . w)           (cons (lambda vardic                     (cons  (cons x (cons y (cons z '()))) (cons w vardic)))                (lambda (foo goo boo)                     (cons goo w))))) (let* ((a (foo 1 2 3 4 5))       (b ((car a)))        (c ((cdr a) 'moshe 'moshe 'moshe)))     (if b c))" ,"expected":"1"},

    {"name":"test111", "folder":"111", "input":"(((lambda (a b . c) (lambda (x) (cons x c))) 1 2 3 4 5 6 7) 8)" ,"expected":"1"},


    {"name":"test112", "folder":"112", "input":"(define list-rev (let ((null? null?)                         (append append))     (lambda (lst)         (if (null? lst)             '()             (append (list-rev (cdr lst)) (cons (car lst) '()))))))" ,"expected":"1"},

    {"name":"test113", "folder":"113", "input":"(define foo (lambda (x y) (+ x y))) (apply foo '(1 2))" ,"expected":"1"},

    {"name":"test114", "folder":"114", "input":"(define list (lambda x x)) (define foo (lambda (x y z w)     (list x y z w))) (apply foo 1 'b '(c 4))" ,"expected":"1"},

    
    {"name":"test115", "folder":"115", "input":"(apply list 1 2 '(3 4 5))" ,"expected":"1"},


    {"name":"test116", "folder":"116", "input":"(define list (lambda x x)) (apply list '(3 4 5))" ,"expected":"1"},


    {"name":"test117", "folder":"117", "input":"(append '(1) '(2 3))" ,"expected":"1"},

    {"name":"test118", "folder":"118", "input":"(string->list \"moshe\")" ,"expected":"1"},

    {"name":"test119", "folder":"119", "input":"(string-ref \"hellp\" 2)" ,"expected":"1"},


    {"name":"test120", "folder":"120", "input":"(string-length \"a\")" ,"expected":"1"},


    {"name":"test121", "folder":"121", "input":"(- 1 2 3 4)" ,"expected":"1"},


    {"name":"test122", "folder":"122", "input":"(< 2 3 4 5)" ,"expected":"1"},

    {"name":"test123", "folder":"123", "input":"(< 2 3 4 1)" ,"expected":"1"},

    {"name":"test124", "folder":"124", "input":"(apply - 0 `(3 ,@(append '(4 4) '(4))))" ,"expected":"1"},


    {"name":"test125", "folder":"125", "input":"(let ((x 3))     (set! x 'ok)     x)" ,"expected":"1"},

    {"name":"test126", "folder":"126", "input":"(let* ((x 3)        (y 5))    (set! x 'is)    (set! y 'fine)    (cons x (cons y '())))" ,"expected":"1"},

    {"name":"test127", "folder":"127", "input":"((((lambda (x) (x (x x)))    (lambda (x) (lambda (y) (x (x y)))))  (lambda (x)     (cons x '())))  'moshe)" ,"expected":"1"},

    {"name":"test128", "folder":"128", "input":"(let ((q 1))   (let ((a 2) (z 3))     (let ((w 4) (s 5) (x 6))       (let ((e 7) (d 8)) (let ((c 9))  (list q a z w s x e d c))))))" ,"expected":"1"},

    {"name":"test129", "folder":"129", "input":"(let ((q 1))   (let ((a 2) (z 3))     (let ((w 4)) (list q a z w))))" ,"expected":"1"},

    {"name":"test130", "folder":"130", "input":"(let* ((x 1)         (y 2)         (z 3)         (w 4))         (cons x (cons y (cons z (cons w '())))))" ,"expected":"1"},

    {"name":"test131", "folder":"131", "input":"(cdr (cons     (let ((q1 1))  (let ((a1 2) (z1 3))    (let ((w1 4)) (list q1 a1 z1 w1))))    (let ((q2 1))    (let ((a2 2) (z2 3))        (let ((w2 4) (s2 5) (x2 6))        (let ((e2 7) (d2 8))    (let ((c2 9))    (list q2 a2 z2 w2 s2 x2 e2 d2 c2)))))))) (let ((q 1))  (let ((a 2) (z 3))    (let ((w 4) (b 5) (c 6)) (list q a z w b c))))" ,"expected":"1"},

    {"name":"test132", "folder":"132", "input":"(define foo (lambda (x y) 	(cons (lambda (y) 		(set! x 5) 		(+ x y)) 	(+ x y)))) (let ((a (foo 1 2)))     (begin ((car a) 0) (cdr a)))" ,"expected":"1"},

    {"name":"test133", "folder":"133", "input":"((define fact   (let ((x (lambda (x)	     ((x (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))	      (lambda (x) (lambda (y) x)))))	(->	 ((lambda (x) (x x))	  (lambda (->)	    (lambda (n)	      (if (zero? n)		  (lambda (x) (lambda (y) y))		  (let ((z ((-> ->) (- n 1))))		    (lambda (x)		      (lambda (y)			(x ((z x) y)))))))))))    (lambda (n)      ((((((((x (x (x (x x)))) (((x (x (x (x x)))) ((x (x (x x))) (x      (x (x (x x)))))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x x      ))))) (x (x (x (x x))))))) ((x (x (x x))) (x (x (x x))))) ((((      (x (x (x (x x)))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x      (x x)))))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x x)))))      (x (x (x (x x))))))) ((x (x (x x))) (x (x (x x))))) (((((x (x      (x (x x)))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x))      )))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x x))))) (x (x      (x (x x))))))) ((x (x (x x))) (x (x (x x))))) (((x (x (x (x x)      ))) (x (x (x x)))) (x (x (x x))))) (((x (x (x(x x)))) (((((x (      x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (x (x (x x)))      ) (((x (x (x (x x)))) ((x (x (x x))) (((x(x (x (x x)))) (((x (      x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x (x      x)))) ((x (x (x x))) (x (x (x x))))) (x(x (x (x x))))))) ((x (      x (x x))) (x (x (x x))))))) ((((x (x(x (x x)))) (((x (x (x (x      x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x (x x)))) (      (x (x (x x))) (x (x (x x))))) (x(x (x (x x))))))) ((x (x (x x)      )) (x (x (x x))))) (((x (x (x (x x)))) (x (x (x x)))) (x (x (x      x))))))) (((((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))      ))) (x (x (x x)))) ((x (x(x (x x)))) (((x (x (x (x x)))) ((x (      x (x x))) (x (x (x (x x)))))) (x (x (x x)))))) (((((x (x (x (x      x)))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (      ((x (x (x (x x)))) ((x (x (x x))) (x (x (x x))))) (x (x (x (x      x))))))) ((x (x (x x))) (x (x (x x))))) (((x (x (x (x x)))) (x      (x (x x)))) (x (x (x x))))) (x (x (x x))))))) (((x (x (x (x x)      ))) (((((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (      x(x (x x)))) (((x(x (x (x x)))) ((x (x (x x))) (x (x (x (x x))      )))) (x (x (x x))))) (((((x (x (x (x x)))) (((x (x (x (x x))))      ((x (x (x x)))(x (x (x (x x)))))) (((x (x (x (x x)))) ((x (x (      x x))) (x (x(x x))))) (x (x (x (x x))))))) ((x (x (x x))) (x (      x (x x)))))(((x (x (x (x x)))) (x (x (x x)))) (x (x (x x)))))      (x (x (x x)))))) (((((x (x (x (x x)))) (((x (x (x (x x)))) ((x      (x (x x)))(x (x (x (x x)))))) (((x (x (x (x x)))) ((x (x (x x)      )) (x (x(x x))))) (x (x (x (x x))))))) ((x (x (x x))) (x (x (x      x)))))(((x (x (x (x x)))) (x (x (x x)))) (x (x (x x))))) ((x (      x (x x))) (((x (x (x (x x)))) (x (x (x x)))) (x (x (x x)))))))      )))(((((x (x (x (x x)))) ((x (x (x x))) (((x (x (x (x x)))) ((      (x(x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x      (x x)))) ((x (x (x x))) (x (x (x x))))) (x (x (x (x x)))))))((      x (x (x x))) (x (x (x x))))))) ((((x (x (x (x x)))) (((x (x(x      (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x (x x)))      )((x (x (x x))) (x (x (x x))))) (x (x (x (x x))))))) ((x(x (x      x))) (x (x (x x))))) (((x (x (x (x x)))) (x (x (x x))))(x (x (      x x)))))) (((x (x (x (x x)))) (((x (x (x (x x)))) ((x (x (x x)      ))(x (x (x (x x)))))) (x (x (x x))))) ((x (x (x x)))(((x (x (x      (x x)))) (x (x (x x)))) (x (x (x x))))))) (((x (x(x (x x)))) (      ((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (x (x (x      x))))) ((x (x (x x))) (((x (x (x (x x)))) (x(x (x x)))) (x (x      (x x))))))))) ((x (x (x x))) (((x (x (x (x x)))) (x (x (x x)))      )(x (x (x x))))))	 (-> n))	(lambda (x) (+ x 1))) 0)))) (fact 5)" ,"expected":"1"},

    {"name":"test134", "folder":"134", "input":"(define with (lambda (s f) (apply f s))) (define crazy-ack   (letrec ((ack3	    (lambda (a b c)	      (cond	       ((and (zero? a) (zero? b)) (+ c 1))	       ((and (zero? a) (zero? c)) (ack-x 0 (- b 1) 1))	       ((zero? a) (ack-z 0 (- b 1) (ack-y 0 b (- c 1))))	       ((and (zero? b) (zero? c)) (ack-x (- a 1) 1 0))	       ((zero? b) (ack-z (- a 1) 1 (ack-y a 0 (- c 1))))	       ((zero? c) (ack-x (- a 1) b (ack-y a (- b 1) 1)))	       (else (ack-z (- a 1) b (ack-y a (- b 1) (ack-x a b (- c 1))))))))	   (ack-x	    (lambda (a . bcs)	      (with bcs		(lambda (b c)		  (ack3 a b c)))))	   (ack-y	    (lambda (a b . cs)	      (with cs		(lambda (c)		  (ack3 a b c)))))	   (ack-z	    (lambda abcs	      (with abcs		(lambda (a b c)		  (ack3 a b c))))))    (lambda ()      (and (= 7 (ack3 0 2 2))	   (= 61 (ack3 0 3 3))	   (= 316 (ack3 1 1 5))	   (= 636 (ack3 2 0 1))	   )))) (crazy-ack)" ,"expected":"1"},

    {"name":"test135", "folder":"135", "input":"(((((lambda (x) (x (x x)))     (lambda (x)      (lambda (y)	(x (x y)))))   (lambda (p)     (p (lambda (x)	  (lambda (y)	    (lambda (z)	      ((z y) x)))))))  (lambda (x)    ((x #t) #f))) (lambda (x)   (lambda (y)     x)))" ,"expected":"1"},

    {"name":"test136", "folder":"136", "input":"(define test   (let ((p1 (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)	      (lambda (z)		(z x2 x3 x4 x5 x6 x7 x8 x9 x10 x1))))	(s '(a b c d e f g h i j)))    (lambda ()       (equal? (((((((((((apply p1 s) p1) p1) p1) p1) p1) p1) p1) p1) p1) 	       list)	      s))))" ,"expected":"1"},

    {"name":"test137", "folder":"137", "input":"(let ((a 1))  (let ((b 2) (c 3))    (let ((d 4) (e 5) (f 6))      (= 720 (* a b c d e f)))))" ,"expected":"1"},

    {"name":"test138", "folder":"138", "input":"(define foo '(1 . 2)) (set-car! foo #\\a) foo (set-cdr! foo #\\nul) foo" ,"expected":"1"},

    {"name":"test139", "folder":"139", "input":"(let ()   ((lambda s     (let ()       ((lambda s s) s s s)))   #t))" ,"expected":"1"},

    {"name":"test140", "folder":"140", "input":"(define with (lambda (s f) (apply f s))) (define fact   (letrec ((fact-1	    (lambda (n r)	      (if (zero? n)		  r		  (fact-2 (- n 1)			  (* n r)			  'moshe			  'yosi))))	   (fact-2	    (lambda (n r _1 _2)	      (if (zero? n)		  r		  (fact-3 (- n 1)			  (* n r)			  'dana			  'michal			  'olga			  'sonia))))	   (fact-3	    (lambda (n r _1 _2 _3 _4)	      (if (zero? n)		  r		  (fact-1 (- n 1)			  (* n r))))))    (lambda (n)      (fact-1 n 1))))" ,"expected":"1"},


    {"name":"test141", "folder":"141", "input":"(define with (lambda (s f) (apply f s))) (define fact-1  (lambda (n)    (if (zero? n)	(list 1 'fact-1)	(with (fact-2 (- n 1))	  (lambda (r . trail)	    (cons (* n r)	      (cons 'fact-1 trail))))))) (define fact-2  (lambda (n)    (if (zero? n)	(list 1 'fact-2)	(with (fact-3 (- n 1))	  (lambda (r . trail)	    (cons (* n r)	      (cons 'fact-2 trail))))))) (define fact-3   (lambda (n)    (if (zero? n)	(list 1 'fact-3)	(with (fact-1 (- n 1))	  (lambda (r . trail)	    (cons (* n r)	      (cons 'fact-3 trail))))))) (fact-1 10)" ,"expected":"1"},
    {"name":"test142", "folder":"142", "input":" (let ((x #f))  (let ()    x)) (let ((x #f) (y #t))  (let ((x #f))    (let ((x #f) (z #f) (t #f))      (let ((x #f) (t #f))	y)))) ((((lambda (x)     (lambda (y)       y))   (lambda (p)     (p (lambda (x y)	  (lambda (p)	    (p y x))))))  (lambda (z) (z #t #f))) (lambda (x y) x)) ((((lambda (x)     (lambda (y)       (x y)))   (lambda (p)     (p (lambda (x y)	  (lambda (p)	    (p y x))))))  (lambda (z) (z #t #f))) (lambda (x y) x)) ((((lambda (x)      (lambda (y)       (x (x y))))   (lambda (p)     (p (lambda (x y)	  (lambda (p)	    (p y x))))))  (lambda (z) (z #t #f))) (lambda (x y) x)) (((((lambda (x) ((x x) (x x)))    (lambda (x)      (lambda (y)	(x (x y)))))   (lambda (p)     (p (lambda (x y)	  (lambda (p)	    (p y x))))))  (lambda (z) (z #t #f))) (lambda (x y) x))" ,"expected":"1"},


    {"name":"test143", "folder":"143", "input":"(cons* '())" ,"expected":"1"},

    {"name":"test144", "folder":"144", "input":"(cons* '(1 . 2))" ,"expected":"1"},

    {"name":"test145", "folder":"145", "input":"(cons* 1 2)" ,"expected":"1"},

    {"name":"test146", "folder":"146", "input":"(cons* 1 2 3)" ,"expected":"1"},

    {"name":"test147", "folder":"147", "input":"(let () 4)" ,"expected":"1"},

    {"name":"test148", "folder":"148", "input":"(length '(1 2 3))" ,"expected":"1"},


    {"name":"test149", "folder":"149", "input":"(fold-left cons '() '(1 2 3 4))" ,"expected":"1"},


    {"name":"test150", "folder":"150", "input":"(fold-left (lambda (a x) (+ a (* x x))) 0 '(1 2 3 4 5))" ,"expected":"1"},

    {"name":"test151", "folder":"151", "input":"(fold-right cons '() '(1 2 3 4))" ,"expected":"1"},


    {"name":"test152", "folder":"152", "input":"(fold-right (lambda ( x a) (+ a (* x x))) 0 '(1 2 3 4 5))" ,"expected":"1"},

    {"name":"test153", "folder":"153", "input":"(append '(a b c) '())" ,"expected":"1"},
    
    {"name":"test154", "folder":"154", "input":"(append '() '(a b c))" ,"expected":"1"},

    {"name":"test155", "folder":"155", "input":"(append '(a b) '(c d))" ,"expected":"1"},

    {"name":"test156", "folder":"156", "input":"(let ((x (list 'b))) (eq? x (cdr (append '(a) x))))" ,"expected":"1"},
    
    {"name":"test157", "folder":"157", "input":"(apply + '(4 5))" ,"expected":"1"},

    {"name":"test158", "folder":"158", "input":"(define min     (lambda args        (fold-left (lambda (x y) (if (< x y)                                        x                                        y)) (car args) args))) (apply min '(6 8 3 2 5)) (apply min  5 1 3 '(6 8 3 2 5))" ,"expected":"1"},
    
    {"name":"test159", "folder":"159", "input":"(define first   (lambda (l)    (apply (lambda (x . y) x)             l))) (define rest  (lambda (l)    (apply (lambda (x . y) y) l)))(first '(a b c d)) (rest '(a b c d))" ,"expected":"1"},

    {"name":"test160", "folder":"160", "input":"(list) (list 1 2 3) (list 3 2 1)" ,"expected":"1"},




    {"name":"test161", "folder":"161", "input":"(list? '()) (list? '(a b c)) (list? 'a) (list? '(3 . 4)) (list? 3)" ,"expected":"1"},

    {"name":"test162", "folder":"162", "input":"(length '())" ,"expected":"1"},
    
    {"name":"test163", "folder":"163", "input":"(make-string 0) (make-string 0 #\\x) (make-string 5 #\\x)" ,"expected":"1"},

    {"name":"test164", "folder":"164", "input":"(not #f) (not #t) (not '()) (not (< 4 5))" ,"expected":"1"},

    {"name":"test165", "folder":"165", "input":"(number? 5) (number? 1.232) (number? #\\a) (number? '(1 2 3)) ","expected":"1"},

   
    {"name":"test166", "folder":"166", "input":"(((((lambda (x)     (lambda (y)      (lambda (z)        (lambda w          (append x y z w))))) '(a)) '(b)) '(c)) 'd 'e)" ,"expected":"1"},

    {"name":"test167", "folder":"167", "input":"(define foo (lambda (x) 	(let ((x (+ x 1)))		(lambda (y)			(let ((y (+ y 1)))				(lambda (z) 					(+ x y z)))))))					(define f1 (foo 1)) (define f12 (f1 2)) (f12 3)		" ,"expected":"1"},

    {"name":"test168", "folder":"168", "input":"(define length 	(lambda (l)		(if (null? l) 0			(+ 1 (length (cdr l)))))) (= (length ''''''''a) (length ``````````a))" ,"expected":"1"},
    
    {"name":"test169", "folder":"169", "input":"(define str (make-string 5 #\\space)) (string-set! str 0 #\\t) (string-set! str 1 #\\tab) (string-set! str 3 #\\n) (string-set! str 4 #\\a) str","expected":"1"},

    {"name":"test170", "folder":"170", "input":"((lambda (a1 a2 a3 a4 a5)     ((lambda (b1 b2 b3 b4)        ((lambda (c1 c2 c3)           ((lambda (d1 d2)              ((lambda (e1)                  e1) d1)) c1 c2)) b1 b2 b3)) a1 a2 a3 a4))    1 2 3 4 5)" ,"expected":"1"},

    {"name":"test171", "folder":"171", "input":"(((lambda (x) 	(lambda (y . z) 		(if `notbool (+ x  (* y (car z)) (car (cdr z)))))) 9) 10 11 12)" ,"expected":"1"},

    {"name":"test172", "folder":"172", "input":"((lambda () ((lambda (a b c d e) e) 'a 'b 'c 'd 'e)))" ,"expected":"1"},

    {"name":"test173", "folder":"173", "input":"((lambda (x . y)     ((lambda ()        (set-cdr! x y)))     x)   (cons 1 2))" ,"expected":"1"},

    {"name":"test174", "folder":"174", "input":"((lambda x    ((lambda (_) '())    (set-car! x 'd))   x) 'a 'b 'c)" ,"expected":"1"}

]




number_of_failed_tests = 0
failed_tests = []
for i in range(1,175):
    #need to check 96
    if (i == 7) or (i==60) or (i==67) or (i==69) or (i==78) or (i==77) or (i==71) or (i==72) or (i==81) or (i == 82) or (i == 86) or (i == 88) or (i == 91) or (i==92) or (i==93) or (i==95) or (i==96) or (i==97) or (i==133) or (i==141) or (i==150):
        continue
    try:
        current_file = open(all_tests[i]["name"]+".scm","w+")
        current_file.write(all_tests[i]["input"])
        current_file.close()
        os.system("make -f ./{0}/Makefile {1}".format(directory,all_tests[i]["name"]))
        os.system("./{0} > {1}".format(all_tests[i]["name"],"compiler_output"+str(i)+".txt"))
        os.system("(scheme -q < {0}.scm) > scheme_output_{1}.txt".format(all_tests[i]["name"],str(i)))
        compiler_output_i = open("compiler_output"+str(i)+".txt","r")
        compiler_output_i_string = compiler_output_i.read()
        compiler_output_i.close()

        scheme_output_i = open("scheme_output_"+str(i)+".txt","r")
        scheme_output_i_string = scheme_output_i.read()
        scheme_output_i.close()


        if(compiler_output_i_string.strip() == scheme_output_i_string.strip()):
            print("Test #{0} Passed".format(str(i)))
        else:
            print("*"*20 + "\n Test#{0} Failed:\n output was: {1}\n expected: {2}\n".format(str(i),compiler_output_i_string,scheme_output_i_string ) + "*"*20)
            number_of_failed_tests +=1
            failed_tests.append(i)

        try:
            os.mkdir(path_tests_folder+"/"+all_tests[i]["folder"])
        except OSError:
            print ("Creation of the directory %s failed" % path_tests_folder)
        shutil.move("compiler_output"+str(i)+".txt",path_tests_folder+"/"+all_tests[i]["folder"])
        shutil.move("scheme_output_"+str(i)+".txt",path_tests_folder+"/"+all_tests[i]["folder"])
        shutil.move(all_tests[i]["name"]+".scm",path_tests_folder+"/"+all_tests[i]["folder"])
        shutil.move(all_tests[i]["name"],path_tests_folder+"/"+all_tests[i]["folder"])
        shutil.move("{0}/{1}/{2}{3}".format(os.getcwd(),directory,all_tests[i]["name"],".o"),path_tests_folder+"/"+all_tests[i]["folder"])
        shutil.move("{0}/{1}/{2}{3}".format(os.getcwd(),directory,all_tests[i]["name"],".s"),path_tests_folder+"/"+all_tests[i]["folder"])


    except Exception as e:
        print("ERROR IN TEST #{}".format(str(i)))
        number_of_failed_tests +=1
        failed_tests.append(i)
if(number_of_failed_tests == 0):
    print("#"*30)
    print("ALL TESTS PASSED!!!!!")
    print("#"*30)
else:
    print("#"*30)
    print("Failed {} Tests number:".format(str(number_of_failed_tests))),
    for j in failed_tests:
        if j is ():
            print(),
        else:
            print(" {} ".format(str(j))),
    print()
    print("#"*30)
