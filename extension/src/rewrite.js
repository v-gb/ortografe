"use strict";

function memo(f) {
    let state = null;
    return function () {
        if (state == null) {
            state = f();
        }
        return state;
    }
}

function parse_digrams(str, special_cases) {
    const map = new Map()
    let total = 0
    for (const line of str.split('/')) {
        if (line) {
            const [digram, count_str] = line.split(' ')
            const count = parseInt(count_str)
            total += count
            map.set(digram, count)
        }
    }
    const entropymap = new Map()
    for (const [k, v] of map) {
        entropymap.set(k, -(Math.log2(v) - Math.log2(total)))
    }
    for (const [k, v] of special_cases) {
        entropymap.set(k, v)
    }
    return entropymap
}

// Including l' and d' helps categorize texts as french more often. The absolute
// values of the entropies don't matter, what matters is the difference between
// the French and English tables. Here we're saying l' is 16x more likely in French,
// which is probably an understatement.
const digrams = memo(() => [parse_digrams(digrams_fr_raw, [["d'", 0.1], ["l'", 0.1]]),
                            parse_digrams(digrams_en_raw, [["d'", 4.1], ["l'", 4.1]])])

// http://practicalcryptography.com/media/cryptanalysis/files/french_bigrams.txt
// http://practicalcryptography.com/media/cryptanalysis/files/english_bigrams.txt
// tr '\n' '/' < french_bigrams.txt | sed -e 's/./\L\0/g'
const digrams_fr_raw = "es 8381853/le 5975929/de 5822318/en 5651557/on 4892107/nt 4867082/re 4653646/an 3695208/la 3592452/er 3485388/te 3418740/el 3309871/se 3126584/ti 2996412/ur 2897720/et 2773914/ne 2765195/is 2698022/ed 2688231/ou 2682629/ar 2529542/in 2509881/it 2481704/st 2469621/qu 2423740/ns 2349565/ai 2333799/me 2285689/ra 2274898/ie 2190688/em 2167130/co 2160913/ue 2104008/tr 2098580/ta 2045894/ce 2023771/ec 2013228/il 1997934/eu 1990886/un 1933444/ss 1917483/nd 1889188/ri 1852139/at 1832195/sa 1816257/io 1791796/al 1783785/pa 1761573/sd 1759684/au 1727049/ep 1695928/si 1678406/so 1677613/or 1659271/li 1639418/ro 1544044/po 1469596/ll 1450838/pr 1437024/us 1402908/ve 1392610/ma 1379383/ui 1377703/nc 1329610/na 1303923/du 1301796/rs 1299077/td 1289198/da 1257150/ir 1243102/ut 1233111/sp 1233006/om 1214099/ch 1211483/rt 1183495/ea 1168753/ac 1143111/ee 1142498/oi 1136584/té 1130611/ré 1106394/as 1098629/pe 1097971/ts 1073051/su 1064318/sl 1059655/ap 1043040/di 1030017/ni 1005518/lo 1000094/tl 981238/to 973853/sc 968955/ée 953223/rl 928358/tu 926245/no 912651/nn 900804/mi 881916/lu 868590/av 862206/vi 858188/rd 856509/ca 844275/ic 840417/pl 839529/dé 834955/ci 808639/ge 791401/am 778795/ét 755204/mo 746894/ér 721191/és 719878/mp 712460/ag 708195/iq 707124/ct 700588/tt 684354/ef 682568/ux 661587/ul 660556/ol 654484/ha 649829/sé 624894/va 618280/fi 606188/he 603438/tp 602580/mm 580426/éc 576904/do 571729/ad 565121/ev 558949/né 558240/iv 556635/uv 553607/im 545016/bl 538347/fo 534399/rm 533882/ei 528919/os 522777/éd 522591/id 522414/ia 519074/fa 512657/lé 512359/uc 511427/up 509798/ab 508783/ng 506750/gr 506283/rc 491787/rr 490689/sm 471368/ru 461665/tc 439193/ud 428686/br 428664/ig 428172/èr 427877/np 423663/ba 423078/vo 422227/ép 422020/àl 408970/cl 408960/op 408116/ga 407681/um 407574/hi 406961/oc 401578/cu 400096/sq 387104/ex 386893/ot 385585/mb 385003/cr 374804/rn 371589/if 368567/eà 364109/eq 363106/gi 361854/ls 357805/eb 356900/iè 354038/él 350938/nf 348294/pi 342767/mé 341740/eg 340900/pp 339879/sr 334060/af 332373/fr 330904/ip 329714/ua 326822/pu 322793/bi 319948/th 319664/ho 318623/ég 317945/ff 316775/gn 313629/sf 308940/bo 298213/dr 294637/fe 293005/jo 290287/ès 287990/nu 286892/be 284102/od 283050/gu 277930/rè 266992/rg 261911/pé 254176/ph 252833/nl 251379/og 246283/mu 241708/ub 241647/ib 236253/tà 234287/eo 232377/rp 231053/ld 229917/ém 229855/je 229197/én 227231/nv 225727/éa 222420/cc 219347/ob 213964/gé 212194/pt 212014/ej 211134/êt 209974/év 209392/nr 208540/sn 204749/ié 202455/fu 199219/nm 198245/sà 197028/bu 196433/rv 195368/tm 193172/vr 193027/sh 191076/sb 186969/sv 185350/lt 184958/hé 175338/tq 172807/eé 170110/ju 161148/ué 160875/cé 159521/ay 157014/xp 155637/xe 153490/ys 153309/go 153057/ug 152376/sg 151379/ya 146169/nq 146169/gl 145371/eh 144508/éf 143631/vé 142591/oy 138604/of 137522/tf 136860/ps 136774/xi 136583/fé 135364/aq 129038/ja 126247/sy 125268/ye 122850/êm 121976/hu 120261/rb 118855/ae 118426/rf 117608/lp 116657/rq 116620/uf 116549/tn 116089/èm 115418/fl 115132/ov 114414/éq 114309/mê 113043/ds 110747/àc 110383/ix 109104/uj 104710/xt 104693/tb 101561/xd 101547/aé 100679/nb 99552/éo 99093/às 98315/lm 97765/sj 97684/éb 96130/lh 94969/àp 93583/lc 92506/dd 89836/nç 88241/xa 87380/ça 87239/aa 86610/xc 86562/lè 86277/tv 84734/lf 83518/ly 83511/ez 83499/àd 82338/ty 79976/aj 79708/ke 79601/aî 79263/dh 77243/tè 76034/rà 74398/nh 74262/cs 73949/ka 73795/rê 73506/ii 73323/éà 73083/èt 72711/mè 71861/èc 71780/nj 71230/ln 71179/md 70831/tê 70594/lq 70121/ck 69907/yp 69707/où 69632/ôt 69233/cd 68594/ki 68425/oq 66932/cè 66241/oo 65377/tg 65269/ât 63936/àu 63919/ek 62250/bé 62164/bs 61419/ey 61130/ît 61029/ym 60616/oe 60259/wa 59876/lb 59802/tj 59483/lr 58089/ry 57818/èn 57488/lg 57301/ah 57117/pè 57029/uà 56890/ms 56531/hr 55842/ew 55823/nà 55577/fs 55455/œu 55156/éu 54745/ny 54705/iu 54443/hn 53464/uu 53259/yl 51773/az 51132/ao 51071/yc 50881/yn 50864/dè 50598/èg 50589/hy 49813/ak 49713/yo 49449/xs 48956/dm 48421/uo 48396/àm 47917/dl 47661/ço 47341/ze 47318/vu 47231/àt 46307/hè 44989/rô 44303/oa 44287/za 44016/zo 43610/sk 43513/rk 42907/xé 42483/ôl 41145/wi 40740/ko 40021/là 39967/xm 38770/àa 38230/we 37300/éé 37026/éi 36850/dp 36701/lv 36608/èl 36176/yr 35787/ax 35713/rh 35030/yd 34632/uq 34391/fd 32924/pd 32561/nk 32174/cq 32161/cô 32003/oh 31862/ût 31839/cy 31765/èd 31736/èv 31688/oj 31477/rj 31248/ow 31151/ik 31099/bt 30910/éj 30624/zi 30525/gt 30105/yt 30067/bj 30058/àr 29880/gè 29605/ox 29500/dc 29298/gm 29277/iz 29167/sè 28816/gh 28630/cp 28154/oû 27760/âc 27714/xo 27695/àb 27533/în 27412/gs 26886/xl 26844/mn 26075/aç 25928/ok 25899/dy 25737/xf 25513/hâ 25435/àe 25344/yé 25328/nz 25235/uh 25028/gd 24858/tô 24349/xv 24277/mt 23687/aï 23616/aw 23506/aà 23458/my 23347/âg 22901/àf 22774/râ 22708/àn 22625/uk 21979/cm 21940/bb 21924/km 21360/bâ 21259/îl 21186/eç 21178/xq 21062/àv 20969/xr 20919/ùi 20758/ks 20730/hl 20632/hs 20616/mc 20225/ht 20149/hm 19812/ùl 19795/éh 19707/zl 19389/dv 19386/ià 19325/nè 19320/çu 19234/dn 19121/ml 19062/wo 18868/hô 18139/sw 17863/ku 17825/ij 17799/dj 17420/uy 17310/uz 17202/by 17043/kh 16949/gg 16944/dg 16900/vè 16897/bd 16830/dt 16794/tz 16581/lâ 16360/êc 16208/pê 16131/sœ 15665/jà 15455/dê 15179/xu 14993/kr 14644/vê 14641/kl 14591/xb 14585/ft 14554/àg 14535/xn 14438/rç 14188/ji 14067/cf 14054/àh 13990/ôm 13918/db 13856/uê 13693/xh 13627/hd 13325/tk 13248/gy 13199/lk 13063/df 12949/dà 12884/sz 12846/lj 12737/oé 12704/èb 12661/tw 12523/lî 12340/oï 12264/fc 12258/cn 12013/àq 11881/ih 11766/ky 11696/dq 11690/xg 11583/yb 11540/éâ 11455/rw 11247/pc 11188/mr 10886/nœ 10569/àj 10545/nw 10497/ài 10471/fp 10381/oz 10374/zu 10358/sê 10271/yi 10270/yf 10204/gp 10186/yg 10128/èq 10037/xj 9971/yu 9968/ôn 9883/pô 9725/yv 9679/ào 9675/fè 9432/cb 9427/py 9276/zd 9154/àé 9152/uè 8917/ws 8904/zz 8854/wy 8790/jé 8625/gb 8567/xy 8503/cœ 8495/rz 8406/dw 8372/gc 8268/hp 8235/oè 8227/fê 8202/oî 8023/êq 7937/kd 7933/iy 7895/hc 7838/zé 7831/mh 7831/àk 7746/lœ 7725/wn 7687/fm 7656/sx 7649/nê 7210/xà 7184/mf 7078/ùs 7074/ïd 7051/mâ 7042/âl 7018/wh 6921/ël 6897/pm 6888/vd 6759/cg 6721/kp 6602/kn 6515/zs 6486/ùe 6447/cv 6349/ûr 6324/nâ 6273/cà 6261/lô 6255/zp 6176/bc 6119/vs 6036/pf 5926/qa 5776/pn 5728/kt 5657/kg 5633/dû 5613/mg 5518/mq 5496/tâ 5397/mà 5375/ôp 5336/vl 5306/gf 5298/zc 5297/bn 5272/jc 5261/êv 5256/uw 5252/xx 5239/hb 5207/sû 5167/kk 5080/zh 5071/ïs 5054/âm 5041/lw 5028/yq 5018/kc 4930/pâ 4915/ké 4895/cj 4799/hw 4792/mv 4733/sâ 4685/zy 4612/sî 4568/wr 4548/yw 4513/mw 4513/àê 4438/eê 4399/ïq 4390/zv 4383/fn 4321/yh 4297/aë 4296/hf 4224/yà 4214/zm 4170/oë 4102/oà 4064/nx 4059/ên 3987/bm 3973/eœ 3963/lz 3962/ûl 3934/rû 3914/dœ 3894/bp 3834/qi 3806/kb 3781/zr 3758/bè 3708/ïe 3691/vy 3685/iw 3677/wl 3641/éz 3610/tx 3573/ww 3563/êl 3553/yj 3541/fq 3504/hk 3469/zb 3458/kf 3458/fb 3416/yz 3413/yk 3406/ân 3373/œi 3314/dk 3276/zn 3255/fg 3232/pg 3212/gq 3138/ék 3136/hh 3111/gw 3100/pb 3097/gv 3062/cê 3033/zt 3010/ïn 2996/zf 2995/hœ 2990/kw 2985/rx 2981/àw 2978/mk 2975/pv 2967/vj 2964/bh 2952/mj 2918/hv 2845/gk 2833/vp 2817/bv 2784/fà 2760/hq 2756/wd 2710/vc 2696/gâ 2666/hà 2659/iê 2637/eè 2633/eâ 2588/hê 2558/wc 2551/wu 2510/kv 2480/hg 2463/lê 2444/vn 2403/bf 2375/ït 2366/wt 2327/fh 2318/cw 2314/jr 2304/âb 2260/qs 2217/wb 2216/bê 2211/eî 2209/kq 2164/dâ 2136/cz 2134/ïl 2115/dz 2087/qc 2077/ùd 2035/gà 2014/fû 2006/ïc 1989/ùc 1974/câ 1972/aè 1968/qe 1959/dî 1958/fy 1937/hz 1926/fv 1922/hj 1900/js 1895/qp 1893/qm 1886/zg 1870/zq 1839/dô 1838/yè 1828/pq 1798/fj 1780/gê 1762/pà 1761/wm 1745/wk 1721/gj 1721/pç 1712/ïa 1688/wp 1682/ür 1679/vm 1660/jd 1659/vf 1645/îm 1642/ùu 1641/nô 1629/vt 1609/kj 1596/àx 1589/aê 1565/wf 1562/yy 1561/îc 1548/zk 1545/bk 1485/jp 1484/ûà 1476/qd 1466/eï 1460/jn 1443/jm 1436/âs 1418/uë 1415/pk 1407/zà 1405/àz 1368/àà 1335/xk 1333/pj 1318/kà 1286/fk 1279/uï 1269/lx 1228/xw 1206/ùp 1204/nî 1196/fw 1193/gz 1187/bq 1180/ûa 1176/ün 1175/zw 1174/ùo 1140/bg 1134/ùa 1129/xè 1123/jk 1117/vœ 1110/yx 1098/rü 1083/mü 1081/jl 1074/zè 1072/vb 1060/eû 1059/bw 1059/vg 1048/vv 1033/ùt 1027/ql 1016/qf 993/gü 988/mû 960/wj 959/lû 951/ül 944/pw 927/éw 924/vh 921/vq 912/bû 911/zj 891/mœ 886/mô 880/éy 880/vk 876/px 867/bœ 866/ïk 865/jb 857/éç 856/qt 846/ày 844/fü 832/âq 825/qr 814/lü 795/ïm 795/èz 789/mz 788/âr 783/ïv 774/ïb 765/ùv 763/pz 762/bà 758/ûc 757/uî 746/üh 745/aô 744/mx 733/wg 722/ïr 721/ôs 719/ïw 706/jt 705/jj 701/üs 699/qo 698/yê 682/ûn 682/ùr 681/xz 676/bü 651/qj 645/vô 641/üc 641/qé 637/ûs 635/gx 629/ën 628/üb 623/gî 623/wé 610/ùn 605/và 597/qb 590/èp 588/cx 586/qv 585/qg 585/dx 578/bî 577/ïo 569/fâ 563/ët 561/jf 559/tœ 555/ïf 553/wq 544/qn 544/ùj 537/jh 527/ûê 518/ùm 517/üe 517/âp 509/tç 507/iç 503/éê 502/ùf 500/âd 497/qq 496/jg 496/dü 494/ûm 493/üt 489/wv 488/fœ 486/qh 480/bz 479/jy 478/ës 476/tü 473/œt 468/fx 466/yô 462/yû 455/fz 455/êp 443/âh 441/hü 434/ùé 431/jv 427/rî 425/xî 417/éx 415/sç 396/xœ 395/œs 391/éè 391/tî 382/uç 378/ùb 378/œd 378/kz 371/pî 366/ër 366/yâ 363/ïg 362/ûp 340/aâ 329/ôd 318/qà 315/kx 310/iâ 310/eô 308/ôe 307/rœ 304/ùh 301/œc 301/èf 297/üd 294/ïp 294/ûd 292/wà 291/ûe 290/aü 290/kü 285/sü 283/sô 277/kâ 277/iô 275/îr 268/çà 266/oê 265/çe 265/uâ 263/vâ 262/xê 253/ûf 252/wz 251/qû 244/hî 235/ùq 233/jü 230/vw 228/jw 228/kè 221/îd 220/ùg 219/œn 218/àœ 215/îs 214/ëe 213/bx 213/âa 212/ôr 210/wü 208/vx 206/pû 204/æt 201/zü 199/nü 193/èe 192/qk 191/bô 191/ëd 190/âj 188/fî 186/jz 182/vî 181/üg 180/hû 177/âi 176/âe 176/ôk 175/jq 171/vz 170/üm 168/ôc 168/hx 166/wâ 165/éï 161/nû 159/ês 157/ây 156/jâ 154/âv 153/ëa 151/ük 148/mî 148/ùà 147/ræ 147/qâ 146/kô 146/ôa 145/ïé 145/ùk 144/îe 143/ûg 142/qw 142/üi 141/tû 141/zx 138/œl 138/cî 137/iœ 135/æd 133/zâ 128/ôj 127/êr 127/ôg 120/xâ 117/æs 116/ûq 114/îa 114/zê 113/ûb 113/zô 110/të 110/oü 110/jô 109/aÿ 109/ïy 108/gô 108/cæ 106/tæ 103/ïu 102/dæ 102/ïi 101/ôv 100/ær 100/ûk 99/lç 99/æl 98/hç 96/æq 96/uœ 93/iæ 93/âk 93/çi 92/yü 91/ië 91/ëc 91/læ 89/àâ 89/kû 88/wx 87/ëk 87/âw 87/wë 86/aœ 86/jû 84/xæ 82/pæ 82/ôq 82/æc 81/üy 80/oÿ 79/ûi 78/næ 78/jx 78/zî 77/ëp 76/èa 76/ûy 74/oç 71/eü 71/oœ 66/îq 66/êe 66/ûj 65/ïà 65/ëf 65/îp 64/ëm 64/yç 63/üa 63/fë 63/ëv 63/ëq 61/èi 61/qy 60/ôi 60/âu 60/æg 60/sæ 59/üp 58/ôb 58/îy 58/âf 58/ôo 57/ïz 57/eæ 57/væ 56/jè 55/cç 55/âo 55/ûv 53/ôu 53/ëo 53/îb 52/yï 51/qî 51/êd 51/æn 51/ôh 50/jî 50/èy 50/ùy 49/iî 49/ûu 48/üé 48/ôy 48/kç 48/iù 48/éœ 48/îk 47/êa 47/ÿe 46/ùw 46/oâ 46/èu 46/qx 45/æm 45/wî 44/ïè 44/gû 44/üz 43/ùx 43/ÿs 42/ûé 42/uæ 42/së 42/qz 42/æe 42/üf 41/sù 41/ôz 41/kœ 41/kî 41/fô 41/ôf 40/œb 40/âz 40/àç 40/tù 39/kê 39/hæ 39/èk 38/dç 38/pü 37/îv 37/iï 37/ëz 37/æo 37/wè 36/ùz 36/ïj 36/èo 36/êk 36/êg 36/ëà 36/hù 35/ïh 34/bë 34/æv 34/ûz 33/ûh 33/pœ 33/îi 33/ëi 33/æp 33/üv 32/ûo 32/îz 32/îh 32/uÿ 31/rë 31/œr 31/mæ 31/cü 31/çk 31/âî 31/ÿd 30/lï 30/èh 30/çâ 30/æa 30/lë 29/îf 29/dë 29/æu 29/kë 28/îo 28/gœ 28/hë 27/ôà 26/ëb 26/oô 25/dù 25/bæ 25/êo 24/æi 24/àè 24/ÿl 23/éü 23/ëg 23/vû 22/îî 22/éô 22/æf 22/uô 21/ôw 21/lù 21/jæ 21/üq 20/jë 20/êu 20/èé 20/çl 20/yù 19/pë 19/më 19/ëx 19/éî 19/nï 18/mù 18/èj 18/êh 18/çr 18/yœ 17/yî 17/üç 17/qœ 17/në 17/mç 17/êy 17/ëu 17/êf 17/èè 17/êé 17/bù 17/aû 17/aù 17/âé 17/üü 16/œe 16/ïx 16/zû 15/üw 15/rù 15/rï 15/îà 15/èà 15/ââ 15/zç 14/uü 14/œh 14/îj 14/îg 14/eÿ 14/çü 14/çû 14/çc 14/ÿr 13/ÿc 13/ÿa 13/vë 13/üj 13/ôé 13/kæ 13/îâ 13/êi 13/ùç 12/rÿ 12/îé 12/gæ 12/ëj 12/çp 12/æb 12/xç 11/eù 11/çé 11/æk 11/üo 10/ùê 10/îu 10/êà 10/cû 10/çm 10/bï 10/àô 10/àæ 10/âà 10/ÿp 9/xô 9/wû 9/jê 9/îw 9/gù 9/ëé 9/êb 9/çd 9/aæ 9/ÿq 8/wœ 8/wô 8/ùœ 8/tï 8/kï 8/gë 8/êz 8/êj 8/âû 8/ÿn 7/ÿf 7/yë 7/üx 7/œx 7/œp 7/œa 7/iü 7/ëy 7/ëh 7/eë 7/éæ 7/çy 7/çs 7/çë 7/cë 7/ôô 6/œv 6/œo 6/œm 6/jœ 6/fù 6/fç 6/fæ 6/èx 6/çç 6/çb 6/àü 6/àî 6/zë 5/wê 5/üu 5/üà 5/sï 5/qê 5/qè 5/œq 5/œf 5/nù 5/çh 5/çg 5/æj 5/æé 5/ÿo 4/ÿm 4/ÿj 4/ÿg 4/xë 4/qô 4/pï 4/jù 4/hï 4/êx 4/ëw 4/èç 4/dï 4/cù 4/çè 4/æh 4/zÿ 3/zù 3/ÿi 3/ÿé 3/ÿb 3/xù 3/wù 3/uû 3/qü 3/pù 3/œz 3/œj 3/ôâ 3/mÿ 3/mï 3/îô 3/gï 3/gç 3/èw 3/êô 3/èê 3/êâ 3/çz 3/çq 3/çn 3/cï 3/bç 3/ææ 3/âê 3/âç 3/zœ 2/zï 2/ÿt 2/ÿh 2/yæ 2/ÿà 2/xü 2/wç 2/wæ 2/vü 2/vï 2/vç 2/ûw 2/uù 2/ôû 2/ôî 2/œô 2/œk 2/œï 2/œg 2/ôç 2/oæ 2/nÿ 2/lÿ 2/kù 2/iû 2/ïô 2/ïï 2/ïî 2/hÿ 2/êw 2/êî 2/èâ 2/dÿ 2/çî 2/çf 2/âï 2/æà 2/zæ 1/ÿy 1/ÿv 1/ÿu 1/xï 1/wï 1/ùù 1/ûô 1/ùè 1/ûâ 1/tÿ 1/ôx 1/ôù 1/ôï 1/œw 1/œœ 1/œé 1/œà 1/ôë 1/ôè 1/kÿ 1/îx 1/îû 1/ïœ 1/îï 1/éû 1/èù 1/éù 1/èï 1/ëë 1/ëç 1/êç 1/çv 1/çt 1/çê 1/çæ 1/âx 1/æw 1/æœ 1/æï 1/æç 1/"
const digrams_en_raw = "th 116997844/he 100689263/in 87674002/er 77134382/an 69775179/re 60923600/es 57070453/on 56915252/st 54018399/nt 50701084/en 48991276/at 48274564/ed 46647960/nd 46194306/to 46115188/or 45725191/ea 43329810/ti 42888666/ar 42353262/te 42295813/ng 38567365/al 38211584/it 37938534/as 37773878/is 37349981/ha 35971841/et 32872552/se 31532272/ou 31112284/of 30540904/le 30383262/sa 30080131/ve 29320973/ro 29230770/ra 28645577/ri 27634643/hi 27495342/ne 27331675/me 27237733/de 27029835/co 26737101/ta 26147593/ec 25775798/si 25758841/ll 24636875/so 23903631/na 23547524/li 23291169/la 23178317/el 23092248/ma 21828378/di 21673998/ic 21468412/rt 21456059/ns 21306421/rs 21237259/io 21210160/om 21066156/ch 20132750/ot 20088048/ca 19930754/ce 19803619/ho 19729026/be 19468489/tt 19367472/fo 18923772/ts 18922522/ss 18915696/no 18894111/ee 18497942/em 18145294/ac 17904683/il 17877600/da 17584055/ni 17452104/ur 17341717/wa 16838794/sh 16773127/ei 16026915/am 15975981/tr 15821226/dt 15759673/us 15699353/lo 15596310/pe 15573318/un 15237699/nc 15214623/wi 15213018/ut 15137169/ad 14877234/ew 14776406/ow 14610429/ge 14425023/ep 14024377/ai 13974919/ly 13742031/ol 13726491/ft 13696078/os 13596265/eo 13524186/ef 13252227/pr 13191182/we 13185116/do 13120322/mo 12950768/id 12896787/ie 12505546/mi 12168944/pa 12068709/fi 11993833/po 11917535/ct 11888752/wh 11852909/ir 11681353/ay 11523416/ga 11239788/sc 10800636/ke 10650670/ev 10574011/sp 10570626/im 10544422/op 10459455/ds 10429887/ld 10245579/ul 10173468/oo 10168856/su 10031005/ia 10002012/gh 9880399/pl 9812226/eb 9738798/ig 9530574/vi 9380037/iv 9129232/wo 9106647/yo 9088497/rd 9025637/tw 8910254/ba 8867461/ag 8809266/ry 8788539/ab 8775582/ls 8675452/sw 8673234/ap 8553911/fe 8529289/tu 8477495/ci 8446084/fa 8357929/ht 8351551/fr 8339376/av 8288885/eg 8286463/go 8188708/bo 8172395/bu 8113271/ty 8008918/mp 7835172/oc 7646952/od 7610214/eh 7559141/ys 7539621/ey 7528342/rm 7377989/ov 7350014/gt 7347990/ya 7239548/ck 7205091/gi 7103140/rn 7064635/gr 6989963/rc 6974063/bl 6941044/lt 6817273/yt 6714151/oa 6554221/ye 6499305/ob 6212512/db 6106719/ff 6085519/sf 6073995/rr 5896212/du 5861311/ki 5814357/uc 5742385/if 5740414/af 5702567/dr 5701879/cl 5683204/ex 5649363/sm 5580755/pi 5559210/sb 5553684/cr 5514347/tl 5403137/oi 5336616/ru 5330557/up 5306948/by 5232074/tc 5196817/nn 5180899/ak 5137311/sl 4965012/nf 4950333/ue 4927837/dw 4906814/au 4884168/pp 4873393/ug 4832325/rl 4803246/rg 4645938/br 4621080/cu 4604045/ua 4589997/dh 4585765/rk 4491400/yi 4461214/lu 4402940/um 4389720/bi 4356462/ny 4343290/nw 4215967/qu 4169424/og 4163126/sn 4157990/mb 4121764/va 4111375/df 4033878/dd 4001275/ms 3922855/gs 3920675/aw 3918960/nh 3915410/pu 3858148/hr 3843001/sd 3842250/tb 3815459/pt 3812475/nm 3796928/dc 3782481/gu 3768430/tm 3759861/mu 3755834/nu 3732602/mm 3730508/nl 3692985/eu 3674130/wn 3649615/nb 3602692/rp 3588188/dm 3544905/sr 3513808/ud 3499535/ui 3481482/rf 3436232/ok 3397570/yw 3379064/tf 3368452/ip 3348621/rw 3348005/rb 3346212/oh 3254659/ks 3227333/dp 3145043/fu 3138900/yc 3128053/tp 3070427/mt 3055946/dl 3050945/nk 3043200/cc 3026492/ub 2990868/rh 2968706/np 2968126/ju 2924815/fl 2890839/dn 2840522/ka 2833038/ph 2825344/hu 2771830/jo 2721345/lf 2702522/yb 2696786/rv 2692445/oe 2616308/ib 2598444/ik 2585124/yp 2581863/gl 2576787/lp 2543957/ym 2516273/lb 2463693/hs 2462026/dg 2442139/gn 2426429/ek 2411639/nr 2393580/ps 2377036/td 2346516/lc 2328063/sk 2321888/yf 2305244/yh 2291273/vo 2253292/ah 2225270/dy 2218040/lm 2216514/sy 2214270/nv 2194534/yd 2122337/fs 2047416/sg 2043770/yr 2021939/yl 2013939/ws 1988727/my 1949129/oy 1932892/kn 1903836/iz 1865802/xp 1840696/lw 1836811/tn 1782119/ko 1758001/aa 1721143/ja 1712763/ze 1709871/fc 1570791/gw 1567991/tg 1530045/xt 1509969/fh 1507604/lr 1505092/je 1487348/yn 1485655/gg 1468286/gf 1465290/eq 1461436/hy 1446451/kt 1443985/hc 1441057/bs 1409672/hw 1403223/hn 1383958/cs 1381608/hm 1353001/nj 1342735/hh 1329998/wt 1301293/gc 1299541/lh 1274048/ej 1256993/fm 1251312/dv 1238565/lv 1238287/wr 1226755/gp 1215204/fp 1199845/gb 1184377/gm 1178511/hl 1169468/lk 1164186/cy 1145316/mc 1101727/yg 1049082/xi 1024736/hb 1014004/fw 1005903/gy 979804/hp 978649/mw 937621/pm 931225/za 929119/lg 926472/iw 922059/xa 904148/fb 888155/sv 882083/gd 879792/ix 879360/aj 870262/kl 846309/hf 834284/hd 828755/ae 815963/sq 800346/dj 799366/fy 789961/az 768359/ln 752316/ao 749566/fd 748027/kw 719633/mf 715087/mh 710864/sj 704442/uf 701892/tv 698150/xc 697995/yu 695512/bb 689158/ww 674610/oj 661082/ax 660826/mr 660619/wl 657782/xe 653947/kh 650095/ox 650078/uo 649906/zi 644035/fg 637758/ih 610683/tk 610333/ii 607124/iu 576683/tj 559473/mn 558397/wy 553647/ky 553296/kf 537342/fn 534362/uy 531960/pw 530411/dk 525744/rj 518157/uk 514873/kr 507020/ku 506618/wm 505687/km 485617/md 481126/ml 478528/ez 465466/kb 457860/wc 448394/wd 432646/hg 429607/bt 428276/zo 424016/kc 420017/pf 418168/yv 411487/pc 400308/py 396147/wb 394820/yk 391953/cp 382923/yj 378679/kp 375653/pb 369336/cd 358435/ji 357577/uw 352732/uh 339341/wf 336213/yy 332973/wp 321746/bc 320380/aq 315068/cb 298053/iq 291635/cm 285942/mg 285133/dq 283314/bj 282608/tz 280007/kd 277982/pd 273162/fj 269865/cf 267630/nz 266461/cw 257253/fv 244685/vy 233082/fk 228905/oz 228556/zz 221275/ij 219128/lj 218362/nq 217422/uv 212051/xo 211173/pg 211133/hk 210385/kg 209266/vs 204093/hv 197539/bm 191807/hj 189906/cn 188046/gv 186777/cg 181590/wu 180884/gj 176947/xh 166599/gk 163830/tq 159111/cq 157546/rq 156933/bh 154489/xs 154347/uz 153736/wk 148964/xu 147533/ux 144814/bd 141752/bw 140189/wg 139890/mv 136314/mj 134263/pn 131645/xm 127492/oq 122677/bv 120081/xw 119322/kk 118811/bp 115161/zu 113538/rz 113432/xf 113031/mk 111041/zh 107639/bn 106125/zy 105871/hq 101241/wj 99435/iy 98361/dz 98038/vr 96416/zs 94993/xy 94329/cv 94224/xb 94041/xr 90046/uj 88168/yq 87953/vd 85611/pk 83017/vu 82830/jr 80471/zl 80039/sz 79840/yz 78281/lq 77148/kj 76816/bf 75352/nx 74844/qa 73527/qi 73387/kv 73184/zw 68865/wv 63930/uu 63043/vt 62912/vp 62577/xd 60101/gq 59750/xl 59585/vc 59024/cz 57914/lz 57314/zt 56955/wz 52836/sx 50975/zb 50652/vl 49032/pv 48105/fq 47504/pj 47043/zm 46034/vw 45608/cj 41526/zc 41037/bg 40516/js 39326/xg 39289/rx 38654/hz 37066/xx 35052/vm 35024/xn 34734/qw 34669/jp 34520/vn 33082/zd 32906/zr 32685/fz 31186/xv 31117/zp 30389/vh 30203/vb 29192/zf 28658/gz 28514/tx 28156/vf 28090/dx 27413/qb 27307/bk 26993/zg 26369/vg 25585/jc 24770/zk 24262/zn 24241/uq 23386/jm 22338/vv 22329/jd 21903/mq 21358/jh 20960/qs 20847/jt 20408/jb 19380/fx 19313/pq 18607/mz 18271/yx 16945/qt 16914/wq 16245/jj 16085/jw 16083/lx 15467/gx 14778/jn 14452/zv 14339/mx 14250/jk 13967/kq 13905/xk 13651/jf 12640/qm 12315/qh 12273/jl 12149/jg 12023/vk 11469/vj 11432/kz 11192/qc 10667/xj 10629/pz 9697/ql 9603/qo 9394/jv 8925/qf 8778/qd 8678/bz 8132/hx 7526/zj 7167/px 6814/qp 6062/qe 6020/qr 5975/zq 5773/jy 5723/bq 5513/xq 5416/cx 5300/kx 5083/wx 4678/qy 4557/qv 4212/qn 3808/vx 3192/bx 3021/jz 2859/vz 2633/qg 2567/qq 2499/zx 2463/xz 2082/qk 2023/vq 1488/qj 1342/qx 765/jx 747/jq 722/qz 280/"

function probably_en_rather_than_fr(str, base_entropy) {
    // See examples of good/bad characterisations in ../test.
    //
    // This might be improvable by a combination of:
    // - extending digrams to include word start, end or both. Because "an",
    //   "de", "le" are very characteristic
    // - filtering out supposed english words from the intersection that don't
    //   look english at all ("comme", "prix")
    // - trigrams? The list seem quite big, but maybe if I trim the long tail.
    // - grabbing more context when we have very short spans and immediate
    //   neighboring texts, like when a link breaks up a text.
    const [fr, en] = digrams()
    let fr_entropy = 0;
    let en_entropy = 0;
    for (let i = 0; i < str.length - 1; i++) {
        const digram = (str[i] + str[i+1]).toLowerCase();
        // 30 is slighly more than the max entropy we get in the stats above.  It's
        // important to have the same value in both cases, so we don't penalize one
        // language when we encounter unknown digrams.
        fr_entropy += fr.get(digram) || 30;
        en_entropy += en.get(digram) || 30;
    }
    let englishness_entropy = en_entropy + base_entropy - fr_entropy
    const expl = [Math.round(englishness_entropy * 10) / 10]
    if (englishness_entropy > 0 && englishness_entropy - base_entropy < 0) {
        expl.push("base_entropy", base_entropy)
    }
    return [englishness_entropy < 0, expl]
}

// manually included from data/english-freq/erofa_intersection
const erofa_en_intersection_raw = "tell hello matter phone office running million pull ladies happening suppose terrible bill attention village support type innocent dollar roll excellent commander correct challenge photo horrible planning staff style bath attorney official jenny shopping media chatter arrive penny intelligence cigarette effort senior warrant attitude bull exhale commit opposite billion argue collection donna charlotte tunnel teddy gym arrange butter attend attractive pattern boxes comment habit intelligent occasion homicide offense chao chaos math commercial williams drill matches jazz supplie alpha whiskey bitter granny commission communication satellite effective affect phase whip tennis technique kidnapping bonnie pierre khan cherry halloween alliance humble coffin personnel application penalty arrangement psycho villa cycle addition chi affection connections sniff lobby illusion travelling mademoiselle collapse phrase programme ego phoenix bicycle innocence allie ballet arrogant offensive buffalo hobby hostile cottage chopper honorable tanner dynamite pudding interrogation belle accuse bizarre betting corridor embarras ciao addict corruption orchestra whisky batterie canyon efficient chorus syndrome lyric shipping spaghetti horizon curry grill marketing occupation hurricane skipper exhibition collector opposition roller attraction ferry sherry tequila taller morphine rabbi stripper carrier caller immune immigration addiction microphone isabelle humour crashes humiliation hotter scooter apparent paddy collective accusation banner ghetto marathon affirmative apocalypse catastrophe hormone chauffeur immense homo sierra cutter scanner buffet philosopher flatter gamma hypocrite attendant oppose collision commence cameraman kidnapper prototype penthouse troll revolver lynch commandant martyr arrogance immigrant commotion peso latte immature correction ripper pennies imminent diesel commerce babysitting seller zipper supporter terrain bulletin brunette hippie hallucination mammy maths communion sketches gallon gallons immoral pollution lotta offshore scanning thermal marrie alligator graffiti derrick appoint allergie lorry gallant scatter porridge syringe rhino shilling habitat supplier hast alphabet interviewer flashes incorrect risotto kappa cassette intellect cellular portfolio shah bette volleyball roulette dyke bonnet accord narrative trillion collaboration excellence python technologie commando terri oppression pollen mayonnaise rocker mahjong commune innovation latter correspondent interruption ballot exhaustion horoscope commodore trolley impeccable horizontal techno softball installation barricade syphilis accommodation lemme stripping orchestral elle caterpillar blizzard hui ping-pong collier zapping harmonica sheikh lotte jacuzzi reconnaissance terra marshmallow pepperoni cholera vendetta jaffa rapper cannabis pyjama donner loch caddy gille hippo constellation bordeaux attire saxophone prix sioux veto baht attentive rushes ferret peseta wharf cranberry synchro gibbon commute addictive riff omelette tonne affidavit anthrax territorial mannequin corral kebab canne flutter hospice communal pamphlet goddam quitter gazette synagogue sphinx sympathie recycle occurrence jazzy cheddar comma silhouette nylon inhaler pierrot annihilation curriculum dahlia boycott hyper irritable aphrodite hombre cyclone affliction chardonnay regrettable phi reggae chrome corvette inflammation penalties irrigation gimmick krypton piccolo durham barrage clipper yiddish analyse thermo zeppelin hippy lithium confetti gazelle enzyme pedigree correspond pellet torah sophistication delirium dribble trotter referendum agha rapport hormonal allez palette inhibition acupuncture gymnasium manoeuvre physique questionnaire rallie attaché shimmy appétit gramophone deux ballast programmer narration oppressive toffee thane mystique photon apparition mozzarella vaudeville terrier polyester piranha ville paulette incorrigible flipper suffocation strapping occupant lobbying typhus faux suppression hydrant paella balthazar ruffian halle hibernation barracuda kohl alla thermostat soufflé illustration dynamo chlamydia symposium shabbat bitte irritation habitable offset insurrection vermouth inhalation allure violette chromosome poly largo terrifie absinthe affront diffuse souffle placebo mahatma hacienda squatter mollie pizzeria mette fritter backgammon moloch torrent larynx dauphin collage stimuli artefact ortho allo symbiote haddock collusion cameramen tricycle tories arroyo intermittent gaffer scythe paddock aberration technicolor steppe succulent soli strychnine hubris sniffer apartheid aggravation typo affluent gnocchi handball siphon hectare lorries mahdi illumination immersion palle lynx sommer matte jeannette parr volley choral baguette shekel chrono phlox hypertension sphincter accumulation guppy hydrate uppercut sonnet commuter baller mille syllabus lyre sombrero scampi affiliation annal doppler calypso philosophie chiffon bonne maharaja nipper errant hydro seppuku mullah hosanna ammonium harmonie opportune salmonella hymen attache pelle palladium corrosive exhume affirmation raff vinaigrette menthol supposition sympathise effendi applicable hier linoleum pharma commencement stopper foetus admittance hovercraft agathe hiatus cellulite maharajah marquette sylvain cellophane papyrus accuser triathlon exhaustive coeur ricotta baccarat salle phonie calle appropriation somme immobile camorra rho eucalyptus milli corroboration skunks philo trekking krill ranches corrective embarrasse guttural casbah pneumothorax exhumation claymore hypotensive agrippa schizo hyperactive plaît malle terre puffin collette croix jaffe chianti lemming thorax guerre hibiscus tachyon thermite synapse commode banyan julienne kyu grizzlies ayatollah aux graphite skiff credo daimyo hara-kiri phallus inexhaustible photocopie lisette hetman communiqué ferrer serra maître pathos potpourri caddie honore kitsch impresario pharmacie roux collaborative rottweiler rosette pitbull benoît rajah slashes allocation flippant churros corrosion ferries chinchilla vanna incorruptible nympho abattoir habitation approximation grappa croquette edelweiss photocopier trimmer hot-dog statuette physio homophobe favela oxymoron scull thora whist pentothal staffer schilling donne butte desperado camellia apostrophe barra calla narra hypothalamus berthe hermaphrodite valkyries suffragette maye whiskies clapper bodhisattva panna tortellini buller biographie pirouette addendum atoll baffle attrition hypo lycra coccyx sommelier accolade setter quelle thallium manette additive aberrant suffrage harmonium honoré onyx week-end lichen balle asphyxia orchestration colley herbivore allegro menthe affable hidalgo synopsis dynastie morphing opposable harakiri excommunication phosphate mitochondrial hippodrome ahana hyperbole barri tulle gyroscope effusion symphonie wah-wah coefficient hadron inopportune paraphrase dilettante yucca villette catharsis xylophone communique"
const erofa_en_intersection = memo(() => {
    return new Set(erofa_en_intersection_raw.split(' '))
})

function is_capitalized(s) {
    return s && s.charAt(0).toLowerCase() != s.charAt(0)
}

function uncapitalize(s) {
    // should be good enough for french, except perhaps some letters
    // with diacritics
    return s.charAt(0).toLowerCase() + s.slice(1)
}

function capitalize(s) {
    // pass undefined/null through, so it's easier to check on the caller side
    return s ? s.charAt(0).toUpperCase() + s.slice(1) : s;
}

function is_plural(s) {
    return s.endsWith("s")
}

function depluralize(s) {
    return s.slice(0, -1)
}

function pluralize(plurals_in_s, s) {
    // pass undefined/null through, so it's easier to check on the caller side
    return s ? s + plurals_in_s : s
}

function collides_with_english(cache, norm_word) {
    if (!cache) {
        return false;
    }
    if (cache.text_is_en != null) {
        return cache.text_is_en[0];
    } else if (erofa_en_intersection().has(norm_word)) {
        // If we determine a word to be english, and colors are on, the red color will
        // isolate the word into its own chunk of text. IF the page changes, we may
        // then process the word on its own and determine it to be french, which is not
        // correct and is also not consistent with the behavior without coloring. So
        // try to be idempotent by refusing to recategorize such words, like humble.
        cache.text_is_en =
            (!cache.node.nodeValue.includes(' ')
             && cache.node.parentNode.style.backgroundColor == 'red')
            ? [true, 'idempotence']
            : probably_en_rather_than_fr(cache.node.nodeValue, cache.base_entropy);
        if (cache.debug && cache.text_is_en[1] != 'idempotence') {
            console.log('en/fr',
                        norm_word,
                        cache.node.nodeValue,
                        ...cache.text_is_en);
        }
        return cache.text_is_en[0];
    } else {
        return false;
    }
}

function rewrite_word(cache, table, plurals_in_s, word) {
    // unsure if the browser gives any guarantee about NFC vs NFD,
    // because currently we assume diacritics are represented the same
    // in the dict and in the page
    if (table.size == 0) {
        const repl = word.replace("a","X")
        return repl == word ? undefined : repl
    } else {
        // We account for plurals and capitalizations, but we can spuriously rewrite or
        // fail to rewrite homographs, like « Rennes » and « Rennes du Père Noël », or
        // the two meanings of « héroïne ».
        let processed_word;
        let postprocess;
        if (is_capitalized(word)) {
            const lookup = table.get(word);
            if (lookup != null) {
                if (lookup == "") {
                    processed_word = '';
                    postprocess = (x) => x;
                } else {
                    processed_word = word;
                    postprocess = (x) => x;
                }
            } else {
                processed_word = uncapitalize(word);
                postprocess = capitalize;
            }
        } else {
            processed_word = word;
            postprocess = (x) => x;
        }
        let repl = postprocess(table.get(processed_word))
        if (repl && collides_with_english(cache, processed_word)) {
            return null;
        }
        if (repl == null && is_plural(processed_word) && plurals_in_s !== null) {
            const depluralized = depluralize(processed_word)
            repl = postprocess(pluralize(plurals_in_s, table.get(depluralized)))
            if (repl && collides_with_english(cache, depluralized)) {
                return null;
            }
        }
        return repl
    }
}

function make_walk(root) {
    // find all bits in the page in the page but:
    // - don't translate <code>, because any change can break code (example: github)
    // - don't translate notranslate, which is used on mdn for inlined bits of code
    // - don't translate anything user writable, like textarea
    return document.createTreeWalker(
        root,
        NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT,
        (node) => {
            return node.nodeType == 3
                ? NodeFilter.FILTER_ACCEPT :
                (node.classList.contains("notranslate")
                 || node.classList.contains("notranscribe")
                 || node.nodeName == "CODE"
                 || node.nodeName == "SCRIPT"
                 || node.nodeName == "NOSCRIPT" // contains only text when scripting is enabled (~everyone)
                                                // which messes up language detection. It's not clear how to
                                                // process these nodes only when JS is disabled, so ignore
                 || node.nodeName == "STYLE"
                 || node.nodeName == "style" // happens inside <svg>, on reddit
                 || node.nodeName == "TEXTAREA"
                 || node.isContentEditable)
                ? NodeFilter.FILTER_REJECT
                : NodeFilter.FILTER_SKIP
        });
}

function lazy(f) {
    let res = null;
    return () => {
        if (res == null) {
            res = f()
        }
        return res
    }
}

async function detect_language_with_browser_api(browser, lazystr, lang) {
    if (!browser?.i18n?.detectLanguage) {
        return  { res: null, fallback: true }
    }
    const [str, dur] = lazystr();
    if (str.length == 0 || str == 'DuckDuckGo\n') {
        // The initial page load on DuckDuckGo contains almost nothing, but not
        // literally nothing...
        return { res: null, fallback: false }
    }
    const { isReliable, languages } = await browser.i18n.detectLanguage(str)
    // I've seen cases where isReliable:false, which was needlessly conservative.
    //
    // Rewrite even if lang is not the main language, because oftentimes there is a
    // fair amount of english in the doc that distorts even pages that visually
    // contain mostly lang.
    return {
        res: languages.some((l) => l.language == lang && l.percentage >= 30),
        why: [`isReliable:${isReliable}`,
              languages.map((e) => e.language + '=' + e.percentage).join(' '),
              `dom-traversal:${dur}ms`]
    }
}

function detect_language_with_lang_attr(lang) {
    const doclang = document.documentElement.lang;
    if (!doclang) {
        return { res: null, fallback: true }
    }
    // Much simpler and faster than looking at the text, but I think not as good as
    // looking at the language on the page. I feel like something with pure english
    // content and a tiny bit of navbar in lang gets treated as lang.
    return { res: doclang.startsWith(lang), why: [`html.lang:${doclang}`] }
}

function detect_language_custom(lazystr, lang) {
    if (lang != 'fr') {
        return { res: null, fallback: true };
    }
    const [str, dur] = lazystr();
    if (str.length == 0) {
        return { res: null, fallback: false };
    }
    const num_diacritics = str.replace(/[^éèêëàçô]/ug, '').length;
    // 0.5% of diacratics seems like it indicates french quite clearly.  Some pages
    // can have less than that, even.
    return { res : (num_diacritics > 0.005 * str.length),
             why: [`diacritics:${((100 * num_diacritics)/str.length).toFixed(1)}%`,
                   `dom-traversal:${dur}ms`]
           }
}

async function plausibly_lang_once_unlogged(browser, lang, root, debug, force_fallback) {
    const lazystr = lazy(() => {
        const t1 = performance.now();
        const walk = make_walk(root);
        let buf = "";
        while (buf.length < 10000 && walk.nextNode()) {
            if (/[^\s]/.test(walk.currentNode.nodeValue)) {
                buf += walk.currentNode.nodeValue + "\n";
            }
        }
        const t2 = performance.now();
        if (debug) {
            console.log("language detection", buf)
        }
        return [buf, t2 - t1]
    })
    let res = { res: null, fallback : true };
    if (res.res == null && res.fallback && !force_fallback) {
        res = await detect_language_with_browser_api(browser, lazystr, lang);
    }
    if (res.res == null && res.fallback && !force_fallback) {
        res = detect_language_with_lang_attr(lang);
    }
    if (res.res == null && res.fallback) {
        res = detect_language_custom(lazystr, lang);
    }
    if (res.res == null && res.fallback) {
        res = { res: true, why: ['just guessing'] }
    }
    return res.res == null ? null : res
}

async function plausibly_lang_once(browser, lang, root, debug, force_fallback) {
    const t1 = performance.now();
    const res = await plausibly_lang_once_unlogged(browser, lang, root, debug, force_fallback);
    const t2 = performance.now();
    console.log(`page is ${lang}:${res ? res.res : "-"}, ${t2 - t1}ms,`, ...(res ? res.why : []))
    return res ? res.res : res
}

async function plausibly_lang(browser, lang, root, debug, force_fallback) {
    let delay = 100;
    while (true) {
        const res = await plausibly_lang_once(browser, lang, root, debug, force_fallback)
        if (res !== null) {
            return res;
        }
        // for pages that contain nothing like deezer, just wait to figure out the
        // language. If the page contains a placeholder saying "please wait for the page
        // to load or similar, at least that placeholder would inform us of the language,
        // so the fact that we won't run the language detection again seems ok.
        await new Promise((resolve) => {
            const observer = new MutationObserver((_mutations) => {
                observer.disconnect();
                setTimeout(resolve, delay)
                delay = Math.min(delay * 2, 1000)
            })
            observer.observe(root, { childList: true, characterData: true, subtree: true })
        });
    }
}

function rewrite_under(options, table, root){
    let count = 0
    let to_remove = []
    const backgroundColor = options.background_color || options.default_background_color || '#b9f4b9'
    const plurals_in_s = options.plurals_in_s === undefined ? "s" : options.plurals_in_s;
    const walk = make_walk(root);
    const base_entropy = window.location.origin.split('.').pop() == 'fr' ? 10 : 0;
    // In .fr pages, for words that are ambiguous between French and English, tilt the
    // odds of considering them French. A word like dynamite or analyse in lemonde.fr or
    // whatever would then go from being slighly English-favored to slightly
    // French-favored. "10" entropy is arbitrary. In practice, the entropy difference
    // is usually much bigger than 10, so it's only cases where the entropy is uncertain
    // (like when we have an isolated word) that are impacted.
    let n;
    while(n=walk.nextNode()) {
        const cache =
              options.fren
              ? { text_is_en: null, node: n, debug: false, base_entropy }
              : null;
        let regular_text = ""
        // we want to split on words, although there's some ambiguity as to
        // what that means
        // - l'analyse => ["l", "'", "analyse"], otherwise we won't rewrite
        // - indo-européenne => ["indo", "-", "européenne"], otherwise we won't rewrite
        // - truc.com/bidule => ["truc.com/bidule"] but truc. com => ["truc", " ."; "com"]
        // - non breakable space should be a word boundary, same as regular spaces
        // \p{L} is any unicode letter I think: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Regular_expressions/Unicode_character_class_escape
        const words1 = n.nodeValue.split(/(\p{L}+(?:[-:./_0-9?]+\p{L}+)*)/u)
        for (const word1 of words1) {
            const words2 =
                  // if we have a word made purely of letters and dashes (but no slashes,
                  // dots, etc), then actually treat that as multiple words, to handle
                  // the indo-européenne case above
                  word1.includes("-") && /^[-\p{L}]*$/u.test(word1)
                  ? (table.has(word1) // for passe-partout -> passepartout
                     ? [ word1 ]
                     : word1.split(/(-)/))
                  : [ word1 ]
            for (const word of words2) {
                const repl = rewrite_word(cache, table, plurals_in_s, word)
                if (!options.color) {
                    regular_text = regular_text + (repl ? repl : word)
                    continue
                }
                const repl2 =
                      (cache?.text_is_en?.[0]
                       && repl != rewrite_word(null, table, plurals_in_s, word))
                      ? word
                      : repl;
                if (!repl2) {
                    regular_text = regular_text + word
                }
                if (repl2 && regular_text) {
                    count += 1
                    n.parentNode.insertBefore(document.createTextNode(regular_text), n)
                    regular_text = ""
                }
                if (repl2) {
                    count += 1
                    const span = document.createElement('span');
                    span.appendChild(document.createTextNode(repl2));
                    span.style.backgroundColor = cache?.text_is_en?.[0] ? 'red' : backgroundColor;
                    n.parentNode.insertBefore(span, n)
                }
            }
        }
        if (n.nodeValue != regular_text) {
            // don't touch the dom if we don't make changes, to avoid what I think
            // is self-triggering in the mutation observer. And it might be faster
            if (!options.color) {
                count += 1
                n.nodeValue = regular_text
            } else {
                if (regular_text) {
                    count += 1
                    n.parentNode.insertBefore(document.createTextNode(regular_text), n)
                }
                to_remove.push(n)
            }
        }
    }
    for (const n of to_remove) {
        n.remove()
    }
    return count
}

const synchronous_updates = new Map([
    [ 'www.youtube.com', function(options, table, mutations) {
        let to_rewrite = [];
        for (const m of mutations) {
            for (const node of m.addedNodes) {
                const elt = node.nodeType == 3 ? node.parentNode : node;
                if (elt.nodeType == 1 && elt.classList.contains('ytp-caption-segment')) {
                    to_rewrite.push(elt)
                }
            }
        }
        for (const node of to_rewrite) {
            // The separate loop was to see if it would make the screen update faster
            // after the DOM was updated. Doesn't seem to help though, sadly, but maybe
            // the batching is good anyway.
            rewrite_under(options, table, node)
        }
    }]
])

function watch_for_changes(options, table, root) {
    // now, for dynamic pages, like reddit or lemonde, rewrite text from times
    // to times. It seems ~impossible to avoid traversing the whole document when rewriting
    // (because even if we know where changes happen, we might be under a notranslate attribute
    // for instance). It seems simpler to just wait for the page to update, and once things have
    // stabilize look at it (i.e. same thing as would happen if a new page was loaded).
    const debug = options.debug_changes;
    let count = 0;
    let time_of_last_rewrite = -10_000;
    let last_changes = [];
    function timeout_fired(delay, count_when_scheduled) {
        return (async function () {
            // Because we traverse the whole page to do any rewrite, it's probably
            // not ideal in terms of performance to rewrite the page immediately on
            // any tiny change. So what we do is:
            // - if the page has changed in the past 100ms, wait 100ms and try again
            // - otherwise, and if the page has changed a lot, just rewrite it
            // - otherwise, rewrite the page if the last rewrite was at least 5s ago
            if (count == count_when_scheduled && (count > 1000 || delay <= 0)) {
                const t1 = performance.now();
                const num_changes = await rewrite_under(options, table, root);
                const t2 = performance.now();
                console.log(`rewriting after ${count} modifications: ${num_changes} changes in ${t2 - t1}ms`)
                if (debug) {
                    console.log(last_changes.slice(-10))
                    console.log(last_changes.slice(-10).map((m) => m.map((m) => m.addedNodes)))
                    last_changes = []
                }
                time_of_last_rewrite = t2
                count = 0;
            } else {
                setTimeout(timeout_fired(delay - 1, count), 100)
            }
        })
    }
    function record_change(n) {
        if (count == 0) {
            const delay = Math.round((5000 + time_of_last_rewrite - performance.now())/100.)
            if (false) { console.log(`detected changes, delay of ${delay}`) }
            setTimeout(timeout_fired(delay, count + n), 100)
        }
        count += n;
    }
    const synchronous_update = synchronous_updates.get(location.hostname)
    const observer = new MutationObserver((mutations) => {
        if (debug) { last_changes.push(mutations) };
        if (synchronous_update) {
            synchronous_update(options, table, mutations)
        }
        // not clear how to avoid looking at the whole page from times to times
        record_change(mutations.length)
    });
    observer.observe(root, { childList: true, characterData: true, subtree: true });
}

function load_dict(options) {
    let table = new Map()
    if (!options.trivial) {
        // I tried storing a json object into the storage, but that's slower than
        // doing by hand here (currently, for the ortograf.net dictionary, it's
        // 50ms for retrieval + 100ms to build the map. If the storage contains an
        // objects, it's 175ms for retrieval and 30ms or more to build the map.
        const t1 = performance.now();
        const [ dict, separator ] =
              options.rewrite == 'rect1990' ? [ dict_rect1990, '/' ] :
              options.rewrite == 'custom' ? [ (options.custom_dict || ''), '\n' ] :
              [ dict_erofa, '/' ];
        for (const line of dict.split(separator)) {
            const [a,b] = line.split(",")
            if (a != null && b != null) {
                table.set(a, b)
            }
        }
        if (table.size == 0) {
            // table.size is used to indicate the options.trivial elsewhere, so ensure we
            // don't provide such a table
            table.set("f3b0686c-b7b6-11ee-9514-dfbe4f72d338", "wonthapppen")
        }
        const t2 = performance.now();
        console.log(`reading table: ${t2 - t1}ms`)
    }
    return table
}


function sequentialized_and_merged(f) {
    let state = 0; // 0: idle, 1: running, 2: running + need to call again afterwards
    return async function() {
        if (state != 0) { state = 2; return };
        try {
            while (true) {
                state = 1;
                await f(...arguments);
                if (state == 1) { return }
            }
        } finally {
            state = 0;
        }
    }
}

function mirror_and_rewrite(input, dst, options_and_table) {
    const f = sequentialized_and_merged(async () => {
        // ideally, we´d be a bit smarter and not rewrite everything on
        // every single keystroke here
        const [ options, table ] = await options_and_table();
        dst.textContent = input.value;
        rewrite_under(options,table,dst);
        if (input.height < dst.height) {
            // autogrow the textarea, so if you paste a large text, you end up with
            // two long texts side by side, instead the textarea with a scrollbar
            // and the other text with a long paragraph.
            input.height = dst.height;
        }
    })
    input.oninput = f;
    return f;
}

function default_highlighting() {
    // You can check if the user prefers dark mode, and maybe if the site supports dark
    // mode, but you can't really ask "is this site using dark mode now?". So we just
    // check the background color. It would be more principled to put a class on the rewritten
    // text, but good enough for now.
    const color = window.getComputedStyle(document.body).getPropertyValue('background-color');
    const color_components = color.match(/\d+/g);
    if (color_components
        && color_components.length == 3
        && color_components.reduce((a, b) => a + (b | 0),0) < 255 / 2 * 3
       ) {
        return '#106410'
    }
    return '#b9f4b9'
}

function normalize_options(options) {
    if (!options.rewrite) {
        options.rewrite = options.disable ? 'disable' : 'erofa';
    }
    if (!options.default_background_color) {
        options.default_background_color = default_highlighting()
    }
}

function look_for_dictionary(browser) {
    browser.runtime.onMessage.addListener((data, sender, send_response) => {
        if (data == "dictionary-url") {
            if (false) {
                send_response("https://www.sinplegraf.org/DataJS/sinple.js");
            } else {
                const elts = document.getElementsByClassName("orthographe-rationnelle-dict");
                const attr = elts?.[0]?.getAttribute("orthographe-rationnelle-dict-url");
                send_response(attr);
            }
        } else {
            return false;
        }
    });
}

async function extension_main() {
    if (typeof browser == "undefined") {
        globalThis.browser = chrome;
    }
    look_for_dictionary(browser);
    const before_storage = performance.now()
    const options = await browser.storage.local.get(
        ['rewrite', 'disable', 'disable_watch', 'color','trivial', 'debug_changes',
         'debug_language', 'debug_lang_test', 'fren']
    )
    const halfway_storage = performance.now();
    if (options.rewrite === 'custom') {
        const { meta, data } = (await browser.storage.local.get(['custom_dict'])).custom_dict;
        options.custom_dict = data;
        options.lang = meta?.lang;
        if (meta?.supports_repeated_rewrites == false) {
            options.disable_watch = true
        }
        switch (typeof meta?.plurals_in_s) {
        case "boolean":
            options.plurals_in_s = meta.plurals_in_s ? "s" : null
            break
        case "string":
            options.plurals_in_s = meta.plurals_in_s;
        }
    }
    normalize_options(options)
    const after_storage = performance.now();
    console.log(`reading options: ${after_storage - before_storage}ms`
                + ` (${after_storage - halfway_storage}ms for custom dict)`,
                { ...options, custom_dict: "#" + options.custom_dict?.length })
    if (options.rewrite == 'disable') { return };
    const table = load_dict(options);
    // start at document.body, because iterating over document means
    // iterating over <style> nodes even though they contain nothing,
    // and setting their nodeValue messes up pages for some reason
    const root = document.body;
    if (!(await plausibly_lang(browser, (options.lang || 'fr'), root,
                               options.debug_language,
                               options.debug_lang_test))) {
        return
    }
    const num_changes = await rewrite_under(options, table, root);
    const after_rewrite = performance.now()
    console.log(`rewriting all texts: ${num_changes} changes in ${after_rewrite - after_storage}ms`)
    if (!options.disable_watch) {
        watch_for_changes(options, table, root)
    }
}
