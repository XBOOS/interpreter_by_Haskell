{-# OPTIONS_GHC -w #-}
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Data.List (stripPrefix)
import Declare
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (11) = happyShift action_6
action_2 (12) = happyShift action_7
action_2 (13) = happyShift action_8
action_2 (14) = happyShift action_9
action_2 (15) = happyShift action_10
action_2 (17) = happyShift action_11
action_2 (18) = happyShift action_12
action_2 (26) = happyShift action_13
action_2 (30) = happyShift action_14
action_2 (40) = happyShift action_15
action_2 (6) = happyGoto action_4
action_2 (9) = happyGoto action_5
action_2 _ = happyFail

action_3 (42) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_2

action_5 (25) = happyShift action_23
action_5 (26) = happyShift action_24
action_5 (27) = happyShift action_25
action_5 (28) = happyShift action_26
action_5 (29) = happyShift action_27
action_5 (34) = happyShift action_28
action_5 (35) = happyShift action_29
action_5 (36) = happyShift action_30
action_5 (37) = happyShift action_31
action_5 (38) = happyShift action_32
action_5 (39) = happyShift action_33
action_5 (41) = happyShift action_34
action_5 _ = happyReduce_1

action_6 (13) = happyShift action_22
action_6 _ = happyFail

action_7 (13) = happyShift action_21
action_7 _ = happyFail

action_8 (30) = happyShift action_20
action_8 _ = happyReduce_30

action_9 _ = happyReduce_26

action_10 (30) = happyShift action_19
action_10 _ = happyFail

action_11 _ = happyReduce_27

action_12 _ = happyReduce_28

action_13 (12) = happyShift action_7
action_13 (13) = happyShift action_8
action_13 (14) = happyShift action_9
action_13 (15) = happyShift action_10
action_13 (17) = happyShift action_11
action_13 (18) = happyShift action_12
action_13 (26) = happyShift action_13
action_13 (30) = happyShift action_14
action_13 (40) = happyShift action_15
action_13 (9) = happyGoto action_18
action_13 _ = happyFail

action_14 (12) = happyShift action_7
action_14 (13) = happyShift action_8
action_14 (14) = happyShift action_9
action_14 (15) = happyShift action_10
action_14 (17) = happyShift action_11
action_14 (18) = happyShift action_12
action_14 (26) = happyShift action_13
action_14 (30) = happyShift action_14
action_14 (40) = happyShift action_15
action_14 (9) = happyGoto action_17
action_14 _ = happyFail

action_15 (12) = happyShift action_7
action_15 (13) = happyShift action_8
action_15 (14) = happyShift action_9
action_15 (15) = happyShift action_10
action_15 (17) = happyShift action_11
action_15 (18) = happyShift action_12
action_15 (26) = happyShift action_13
action_15 (30) = happyShift action_14
action_15 (40) = happyShift action_15
action_15 (9) = happyGoto action_16
action_15 _ = happyFail

action_16 _ = happyReduce_25

action_17 (25) = happyShift action_23
action_17 (26) = happyShift action_24
action_17 (27) = happyShift action_25
action_17 (28) = happyShift action_26
action_17 (29) = happyShift action_27
action_17 (31) = happyShift action_52
action_17 (34) = happyShift action_28
action_17 (35) = happyShift action_29
action_17 (36) = happyShift action_30
action_17 (37) = happyShift action_31
action_17 (38) = happyShift action_32
action_17 (39) = happyShift action_33
action_17 (41) = happyShift action_34
action_17 _ = happyFail

action_18 _ = happyReduce_24

action_19 (12) = happyShift action_7
action_19 (13) = happyShift action_8
action_19 (14) = happyShift action_9
action_19 (15) = happyShift action_10
action_19 (17) = happyShift action_11
action_19 (18) = happyShift action_12
action_19 (26) = happyShift action_13
action_19 (30) = happyShift action_14
action_19 (40) = happyShift action_15
action_19 (9) = happyGoto action_51
action_19 _ = happyFail

action_20 (12) = happyShift action_7
action_20 (13) = happyShift action_8
action_20 (14) = happyShift action_9
action_20 (15) = happyShift action_10
action_20 (17) = happyShift action_11
action_20 (18) = happyShift action_12
action_20 (26) = happyShift action_13
action_20 (30) = happyShift action_14
action_20 (40) = happyShift action_15
action_20 (9) = happyGoto action_49
action_20 (10) = happyGoto action_50
action_20 _ = happyReduce_34

action_21 (33) = happyShift action_48
action_21 _ = happyFail

action_22 (30) = happyShift action_47
action_22 _ = happyFail

action_23 (12) = happyShift action_7
action_23 (13) = happyShift action_8
action_23 (14) = happyShift action_9
action_23 (15) = happyShift action_10
action_23 (17) = happyShift action_11
action_23 (18) = happyShift action_12
action_23 (26) = happyShift action_13
action_23 (30) = happyShift action_14
action_23 (40) = happyShift action_15
action_23 (9) = happyGoto action_46
action_23 _ = happyFail

action_24 (12) = happyShift action_7
action_24 (13) = happyShift action_8
action_24 (14) = happyShift action_9
action_24 (15) = happyShift action_10
action_24 (17) = happyShift action_11
action_24 (18) = happyShift action_12
action_24 (26) = happyShift action_13
action_24 (30) = happyShift action_14
action_24 (40) = happyShift action_15
action_24 (9) = happyGoto action_45
action_24 _ = happyFail

action_25 (12) = happyShift action_7
action_25 (13) = happyShift action_8
action_25 (14) = happyShift action_9
action_25 (15) = happyShift action_10
action_25 (17) = happyShift action_11
action_25 (18) = happyShift action_12
action_25 (26) = happyShift action_13
action_25 (30) = happyShift action_14
action_25 (40) = happyShift action_15
action_25 (9) = happyGoto action_44
action_25 _ = happyFail

action_26 (12) = happyShift action_7
action_26 (13) = happyShift action_8
action_26 (14) = happyShift action_9
action_26 (15) = happyShift action_10
action_26 (17) = happyShift action_11
action_26 (18) = happyShift action_12
action_26 (26) = happyShift action_13
action_26 (30) = happyShift action_14
action_26 (40) = happyShift action_15
action_26 (9) = happyGoto action_43
action_26 _ = happyFail

action_27 (12) = happyShift action_7
action_27 (13) = happyShift action_8
action_27 (14) = happyShift action_9
action_27 (15) = happyShift action_10
action_27 (17) = happyShift action_11
action_27 (18) = happyShift action_12
action_27 (26) = happyShift action_13
action_27 (30) = happyShift action_14
action_27 (40) = happyShift action_15
action_27 (9) = happyGoto action_42
action_27 _ = happyFail

action_28 (12) = happyShift action_7
action_28 (13) = happyShift action_8
action_28 (14) = happyShift action_9
action_28 (15) = happyShift action_10
action_28 (17) = happyShift action_11
action_28 (18) = happyShift action_12
action_28 (26) = happyShift action_13
action_28 (30) = happyShift action_14
action_28 (40) = happyShift action_15
action_28 (9) = happyGoto action_41
action_28 _ = happyFail

action_29 (12) = happyShift action_7
action_29 (13) = happyShift action_8
action_29 (14) = happyShift action_9
action_29 (15) = happyShift action_10
action_29 (17) = happyShift action_11
action_29 (18) = happyShift action_12
action_29 (26) = happyShift action_13
action_29 (30) = happyShift action_14
action_29 (40) = happyShift action_15
action_29 (9) = happyGoto action_40
action_29 _ = happyFail

action_30 (12) = happyShift action_7
action_30 (13) = happyShift action_8
action_30 (14) = happyShift action_9
action_30 (15) = happyShift action_10
action_30 (17) = happyShift action_11
action_30 (18) = happyShift action_12
action_30 (26) = happyShift action_13
action_30 (30) = happyShift action_14
action_30 (40) = happyShift action_15
action_30 (9) = happyGoto action_39
action_30 _ = happyFail

action_31 (12) = happyShift action_7
action_31 (13) = happyShift action_8
action_31 (14) = happyShift action_9
action_31 (15) = happyShift action_10
action_31 (17) = happyShift action_11
action_31 (18) = happyShift action_12
action_31 (26) = happyShift action_13
action_31 (30) = happyShift action_14
action_31 (40) = happyShift action_15
action_31 (9) = happyGoto action_38
action_31 _ = happyFail

action_32 (12) = happyShift action_7
action_32 (13) = happyShift action_8
action_32 (14) = happyShift action_9
action_32 (15) = happyShift action_10
action_32 (17) = happyShift action_11
action_32 (18) = happyShift action_12
action_32 (26) = happyShift action_13
action_32 (30) = happyShift action_14
action_32 (40) = happyShift action_15
action_32 (9) = happyGoto action_37
action_32 _ = happyFail

action_33 (12) = happyShift action_7
action_33 (13) = happyShift action_8
action_33 (14) = happyShift action_9
action_33 (15) = happyShift action_10
action_33 (17) = happyShift action_11
action_33 (18) = happyShift action_12
action_33 (26) = happyShift action_13
action_33 (30) = happyShift action_14
action_33 (40) = happyShift action_15
action_33 (9) = happyGoto action_36
action_33 _ = happyFail

action_34 (12) = happyShift action_7
action_34 (13) = happyShift action_8
action_34 (14) = happyShift action_9
action_34 (15) = happyShift action_10
action_34 (17) = happyShift action_11
action_34 (18) = happyShift action_12
action_34 (26) = happyShift action_13
action_34 (30) = happyShift action_14
action_34 (40) = happyShift action_15
action_34 (9) = happyGoto action_35
action_34 _ = happyFail

action_35 (25) = happyShift action_23
action_35 (26) = happyShift action_24
action_35 (27) = happyShift action_25
action_35 (28) = happyShift action_26
action_35 (29) = happyShift action_27
action_35 (34) = happyShift action_28
action_35 (35) = happyShift action_29
action_35 (36) = happyShift action_30
action_35 (37) = happyShift action_31
action_35 (38) = happyShift action_32
action_35 (39) = happyShift action_33
action_35 _ = happyReduce_12

action_36 (25) = happyShift action_23
action_36 (26) = happyShift action_24
action_36 (27) = happyShift action_25
action_36 (28) = happyShift action_26
action_36 (29) = happyShift action_27
action_36 (34) = happyShift action_28
action_36 (35) = happyShift action_29
action_36 (36) = happyShift action_30
action_36 (37) = happyShift action_31
action_36 (38) = happyShift action_32
action_36 _ = happyReduce_13

action_37 (25) = happyShift action_23
action_37 (26) = happyShift action_24
action_37 (27) = happyShift action_25
action_37 (28) = happyShift action_26
action_37 (29) = happyShift action_27
action_37 (34) = happyShift action_28
action_37 (35) = happyShift action_29
action_37 (36) = happyShift action_30
action_37 (37) = happyShift action_31
action_37 (38) = happyFail
action_37 _ = happyReduce_14

action_38 (25) = happyShift action_23
action_38 (26) = happyShift action_24
action_38 (27) = happyShift action_25
action_38 (28) = happyShift action_26
action_38 (29) = happyShift action_27
action_38 (34) = happyFail
action_38 (35) = happyFail
action_38 (36) = happyFail
action_38 (37) = happyFail
action_38 _ = happyReduce_18

action_39 (25) = happyShift action_23
action_39 (26) = happyShift action_24
action_39 (27) = happyShift action_25
action_39 (28) = happyShift action_26
action_39 (29) = happyShift action_27
action_39 (34) = happyFail
action_39 (35) = happyFail
action_39 (36) = happyFail
action_39 (37) = happyFail
action_39 _ = happyReduce_16

action_40 (25) = happyShift action_23
action_40 (26) = happyShift action_24
action_40 (27) = happyShift action_25
action_40 (28) = happyShift action_26
action_40 (29) = happyShift action_27
action_40 (34) = happyFail
action_40 (35) = happyFail
action_40 (36) = happyFail
action_40 (37) = happyFail
action_40 _ = happyReduce_17

action_41 (25) = happyShift action_23
action_41 (26) = happyShift action_24
action_41 (27) = happyShift action_25
action_41 (28) = happyShift action_26
action_41 (29) = happyShift action_27
action_41 (34) = happyFail
action_41 (35) = happyFail
action_41 (36) = happyFail
action_41 (37) = happyFail
action_41 _ = happyReduce_15

action_42 (29) = happyShift action_27
action_42 _ = happyReduce_23

action_43 (29) = happyShift action_27
action_43 _ = happyReduce_22

action_44 (29) = happyShift action_27
action_44 _ = happyReduce_21

action_45 (27) = happyShift action_25
action_45 (28) = happyShift action_26
action_45 (29) = happyShift action_27
action_45 _ = happyReduce_20

action_46 (27) = happyShift action_25
action_46 (28) = happyShift action_26
action_46 (29) = happyShift action_27
action_46 _ = happyReduce_19

action_47 (13) = happyShift action_58
action_47 (7) = happyGoto action_57
action_47 _ = happyReduce_7

action_48 (12) = happyShift action_7
action_48 (13) = happyShift action_8
action_48 (14) = happyShift action_9
action_48 (15) = happyShift action_10
action_48 (17) = happyShift action_11
action_48 (18) = happyShift action_12
action_48 (26) = happyShift action_13
action_48 (30) = happyShift action_14
action_48 (40) = happyShift action_15
action_48 (9) = happyGoto action_56
action_48 _ = happyFail

action_49 (25) = happyShift action_23
action_49 (26) = happyShift action_24
action_49 (27) = happyShift action_25
action_49 (28) = happyShift action_26
action_49 (29) = happyShift action_27
action_49 (34) = happyShift action_28
action_49 (35) = happyShift action_29
action_49 (36) = happyShift action_30
action_49 (37) = happyShift action_31
action_49 (38) = happyShift action_32
action_49 (39) = happyShift action_33
action_49 (41) = happyShift action_34
action_49 _ = happyReduce_33

action_50 (21) = happyShift action_54
action_50 (31) = happyShift action_55
action_50 _ = happyFail

action_51 (25) = happyShift action_23
action_51 (26) = happyShift action_24
action_51 (27) = happyShift action_25
action_51 (28) = happyShift action_26
action_51 (29) = happyShift action_27
action_51 (31) = happyShift action_53
action_51 (34) = happyShift action_28
action_51 (35) = happyShift action_29
action_51 (36) = happyShift action_30
action_51 (37) = happyShift action_31
action_51 (38) = happyShift action_32
action_51 (39) = happyShift action_33
action_51 (41) = happyShift action_34
action_51 _ = happyFail

action_52 _ = happyReduce_31

action_53 (12) = happyShift action_7
action_53 (13) = happyShift action_8
action_53 (14) = happyShift action_9
action_53 (15) = happyShift action_10
action_53 (17) = happyShift action_11
action_53 (18) = happyShift action_12
action_53 (26) = happyShift action_13
action_53 (30) = happyShift action_14
action_53 (40) = happyShift action_15
action_53 (9) = happyGoto action_64
action_53 _ = happyFail

action_54 (12) = happyShift action_7
action_54 (13) = happyShift action_8
action_54 (14) = happyShift action_9
action_54 (15) = happyShift action_10
action_54 (17) = happyShift action_11
action_54 (18) = happyShift action_12
action_54 (26) = happyShift action_13
action_54 (30) = happyShift action_14
action_54 (40) = happyShift action_15
action_54 (9) = happyGoto action_63
action_54 _ = happyFail

action_55 _ = happyReduce_29

action_56 (25) = happyShift action_23
action_56 (26) = happyShift action_24
action_56 (27) = happyShift action_25
action_56 (28) = happyShift action_26
action_56 (29) = happyShift action_27
action_56 (32) = happyShift action_62
action_56 (34) = happyShift action_28
action_56 (35) = happyShift action_29
action_56 (36) = happyShift action_30
action_56 (37) = happyShift action_31
action_56 (38) = happyShift action_32
action_56 (39) = happyShift action_33
action_56 (41) = happyShift action_34
action_56 _ = happyFail

action_57 (21) = happyShift action_60
action_57 (31) = happyShift action_61
action_57 _ = happyFail

action_58 (22) = happyShift action_59
action_58 _ = happyFail

action_59 (19) = happyShift action_70
action_59 (20) = happyShift action_71
action_59 (8) = happyGoto action_69
action_59 _ = happyFail

action_60 (13) = happyShift action_68
action_60 _ = happyFail

action_61 (23) = happyShift action_67
action_61 _ = happyFail

action_62 (12) = happyShift action_7
action_62 (13) = happyShift action_8
action_62 (14) = happyShift action_9
action_62 (15) = happyShift action_10
action_62 (17) = happyShift action_11
action_62 (18) = happyShift action_12
action_62 (26) = happyShift action_13
action_62 (30) = happyShift action_14
action_62 (40) = happyShift action_15
action_62 (9) = happyGoto action_66
action_62 _ = happyFail

action_63 (25) = happyShift action_23
action_63 (26) = happyShift action_24
action_63 (27) = happyShift action_25
action_63 (28) = happyShift action_26
action_63 (29) = happyShift action_27
action_63 (34) = happyShift action_28
action_63 (35) = happyShift action_29
action_63 (36) = happyShift action_30
action_63 (37) = happyShift action_31
action_63 (38) = happyShift action_32
action_63 (39) = happyShift action_33
action_63 (41) = happyShift action_34
action_63 _ = happyReduce_32

action_64 (25) = happyShift action_23
action_64 (26) = happyShift action_24
action_64 (27) = happyShift action_25
action_64 (28) = happyShift action_26
action_64 (29) = happyShift action_27
action_64 (32) = happyShift action_65
action_64 (34) = happyShift action_28
action_64 (35) = happyShift action_29
action_64 (36) = happyShift action_30
action_64 (37) = happyShift action_31
action_64 (38) = happyShift action_32
action_64 (39) = happyShift action_33
action_64 (41) = happyShift action_34
action_64 _ = happyFail

action_65 (16) = happyShift action_74
action_65 _ = happyFail

action_66 (25) = happyShift action_23
action_66 (26) = happyShift action_24
action_66 (27) = happyShift action_25
action_66 (28) = happyShift action_26
action_66 (29) = happyShift action_27
action_66 (34) = happyShift action_28
action_66 (35) = happyShift action_29
action_66 (36) = happyShift action_30
action_66 (37) = happyShift action_31
action_66 (38) = happyShift action_32
action_66 (39) = happyShift action_33
action_66 (41) = happyShift action_34
action_66 _ = happyReduce_10

action_67 (12) = happyShift action_7
action_67 (13) = happyShift action_8
action_67 (14) = happyShift action_9
action_67 (15) = happyShift action_10
action_67 (17) = happyShift action_11
action_67 (18) = happyShift action_12
action_67 (26) = happyShift action_13
action_67 (30) = happyShift action_14
action_67 (40) = happyShift action_15
action_67 (9) = happyGoto action_73
action_67 _ = happyFail

action_68 (22) = happyShift action_72
action_68 _ = happyFail

action_69 _ = happyReduce_6

action_70 _ = happyReduce_8

action_71 _ = happyReduce_9

action_72 (19) = happyShift action_70
action_72 (20) = happyShift action_71
action_72 (8) = happyGoto action_77
action_72 _ = happyFail

action_73 (24) = happyShift action_76
action_73 (25) = happyShift action_23
action_73 (26) = happyShift action_24
action_73 (27) = happyShift action_25
action_73 (28) = happyShift action_26
action_73 (29) = happyShift action_27
action_73 (34) = happyShift action_28
action_73 (35) = happyShift action_29
action_73 (36) = happyShift action_30
action_73 (37) = happyShift action_31
action_73 (38) = happyShift action_32
action_73 (39) = happyShift action_33
action_73 (41) = happyShift action_34
action_73 _ = happyFail

action_74 (12) = happyShift action_7
action_74 (13) = happyShift action_8
action_74 (14) = happyShift action_9
action_74 (15) = happyShift action_10
action_74 (17) = happyShift action_11
action_74 (18) = happyShift action_12
action_74 (26) = happyShift action_13
action_74 (30) = happyShift action_14
action_74 (40) = happyShift action_15
action_74 (9) = happyGoto action_75
action_74 _ = happyFail

action_75 (25) = happyShift action_23
action_75 (26) = happyShift action_24
action_75 (27) = happyShift action_25
action_75 (28) = happyShift action_26
action_75 (29) = happyShift action_27
action_75 (34) = happyShift action_28
action_75 (35) = happyShift action_29
action_75 (36) = happyShift action_30
action_75 (37) = happyShift action_31
action_75 (38) = happyShift action_32
action_75 (39) = happyShift action_33
action_75 (41) = happyShift action_34
action_75 _ = happyReduce_11

action_76 _ = happyReduce_4

action_77 _ = happyReduce_5

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happyReduce 8 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2, Function happy_var_4 happy_var_7)
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_1 ++ [(happy_var_3, happy_var_5)]
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn7
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (TInt
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (TBool
	)

happyReduce_10 = happyReduce 6 9 happyReduction_10
happyReduction_10 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Decl happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 8 9 happyReduction_11
happyReduction_11 ((HappyAbsSyn9  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (If happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin Or happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin And happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin EQ happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin LT happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin GT happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin LE happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin GE happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin Add happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  9 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin Sub happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  9 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin Mult happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  9 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin Div happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  9 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Bin Power happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  9 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Unary Neg happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  9 happyReduction_25
happyReduction_25 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Unary Not happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 (HappyTerminal (TokenDigits happy_var_1))
	 =  HappyAbsSyn9
		 (Lit (IntV happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn9
		 (Lit (BoolV True)
	)

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn9
		 (Lit (BoolV False)
	)

happyReduce_29 = happyReduce 4 9 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn9
		 (Var happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  9 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  10 happyReduction_32
happyReduction_32 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  10 happyReduction_33
happyReduction_33 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  10 happyReduction_34
happyReduction_34  =  HappyAbsSyn10
		 ([]
	)

happyNewToken action sts stk [] =
	action 42 42 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenKeyword "function" -> cont 11;
	TokenKeyword "var" -> cont 12;
	TokenIdent happy_dollar_dollar -> cont 13;
	TokenDigits happy_dollar_dollar -> cont 14;
	TokenKeyword "if" -> cont 15;
	TokenKeyword "else" -> cont 16;
	TokenKeyword "true" -> cont 17;
	TokenKeyword "false" -> cont 18;
	TokenKeyword "Int" -> cont 19;
	TokenKeyword "Bool" -> cont 20;
	TokenSymbol "," -> cont 21;
	TokenSymbol ":" -> cont 22;
	TokenSymbol "{" -> cont 23;
	TokenSymbol "}" -> cont 24;
	TokenSymbol "+" -> cont 25;
	TokenSymbol "-" -> cont 26;
	TokenSymbol "*" -> cont 27;
	TokenSymbol "/" -> cont 28;
	TokenSymbol "^" -> cont 29;
	TokenSymbol "(" -> cont 30;
	TokenSymbol ")" -> cont 31;
	TokenSymbol ";" -> cont 32;
	TokenSymbol "=" -> cont 33;
	TokenSymbol "<" -> cont 34;
	TokenSymbol "<=" -> cont 35;
	TokenSymbol ">" -> cont 36;
	TokenSymbol ">=" -> cont 37;
	TokenSymbol "==" -> cont 38;
	TokenSymbol "&&" -> cont 39;
	TokenSymbol "!" -> cont 40;
	TokenSymbol "||" -> cont 41;
	_ -> happyError' (tk:tks)
	}

happyError_ 42 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenDigits Int
      | TokenKeyword String
      | TokenSymbol String
      | TokenIdent String

lexer :: [String] -> [String] -> String -> [Token]
lexer symbols keywords = lexer'
  where lexer' [] = []
        lexer' s@(c:cs)
          | isSpace c = lexer' cs
          | isDigit c = lexNum s
          | isAlpha c = lexVar s
          | otherwise = lexSym s symbols

        lexNum cs = TokenDigits (read num) : lexer' rest
          where (num, rest) = span isDigit cs

        lexVar cs = token : lexer' rest
          where (var, rest) = span isAlpha cs
                token = if var `elem` keywords
                        then TokenKeyword var
                        else TokenIdent var

        lexSym cs (s:ss) = case stripPrefix s cs of
                            Just rest -> TokenSymbol s : lexer' rest
                            Nothing -> lexSym cs ss
        lexSym cs [] = error $ "Cannot tokenize " ++ cs

symbols = ["{", "}", ",", "+", "-", "*", "/", "(", ")", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!", "^", ":"]
keywords = ["function", "var", "if", "else", "true", "false", "Bool", "Int"]

parseExpr = parser . lexer symbols keywords
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}









{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

