{-# OPTIONS_GHC -w #-}
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Data.List (stripPrefix)
import Declare
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

action_0 (13) = happyShift action_2
action_0 (14) = happyShift action_12
action_0 (15) = happyShift action_13
action_0 (16) = happyShift action_14
action_0 (18) = happyShift action_15
action_0 (19) = happyShift action_16
action_0 (21) = happyShift action_17
action_0 (25) = happyShift action_18
action_0 (35) = happyShift action_19
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 (10) = happyGoto action_9
action_0 (11) = happyGoto action_10
action_0 (12) = happyGoto action_11
action_0 _ = happyFail

action_1 (13) = happyShift action_2
action_1 _ = happyFail

action_2 (14) = happyShift action_36
action_2 _ = happyFail

action_3 (37) = happyAccept
action_3 _ = happyFail

action_4 (36) = happyShift action_35
action_4 _ = happyReduce_3

action_5 (34) = happyShift action_34
action_5 _ = happyReduce_5

action_6 _ = happyReduce_7

action_7 (20) = happyShift action_27
action_7 (21) = happyShift action_28
action_7 (29) = happyShift action_29
action_7 (30) = happyShift action_30
action_7 (31) = happyShift action_31
action_7 (32) = happyShift action_32
action_7 (33) = happyShift action_33
action_7 _ = happyReduce_13

action_8 (22) = happyShift action_25
action_8 (23) = happyShift action_26
action_8 _ = happyReduce_16

action_9 _ = happyReduce_19

action_10 (24) = happyShift action_24
action_10 _ = happyReduce_21

action_11 _ = happyReduce_24

action_12 _ = happyReduce_28

action_13 _ = happyReduce_25

action_14 (25) = happyShift action_23
action_14 _ = happyFail

action_15 _ = happyReduce_26

action_16 _ = happyReduce_27

action_17 (14) = happyShift action_12
action_17 (15) = happyShift action_13
action_17 (18) = happyShift action_15
action_17 (19) = happyShift action_16
action_17 (21) = happyShift action_17
action_17 (25) = happyShift action_18
action_17 (35) = happyShift action_19
action_17 (11) = happyGoto action_22
action_17 (12) = happyGoto action_11
action_17 _ = happyFail

action_18 (13) = happyShift action_2
action_18 (14) = happyShift action_12
action_18 (15) = happyShift action_13
action_18 (16) = happyShift action_14
action_18 (18) = happyShift action_15
action_18 (19) = happyShift action_16
action_18 (21) = happyShift action_17
action_18 (25) = happyShift action_18
action_18 (35) = happyShift action_19
action_18 (4) = happyGoto action_21
action_18 (5) = happyGoto action_4
action_18 (6) = happyGoto action_5
action_18 (7) = happyGoto action_6
action_18 (8) = happyGoto action_7
action_18 (9) = happyGoto action_8
action_18 (10) = happyGoto action_9
action_18 (11) = happyGoto action_10
action_18 (12) = happyGoto action_11
action_18 _ = happyFail

action_19 (14) = happyShift action_12
action_19 (15) = happyShift action_13
action_19 (18) = happyShift action_15
action_19 (19) = happyShift action_16
action_19 (21) = happyShift action_17
action_19 (25) = happyShift action_18
action_19 (35) = happyShift action_19
action_19 (11) = happyGoto action_20
action_19 (12) = happyGoto action_11
action_19 _ = happyFail

action_20 _ = happyReduce_23

action_21 (26) = happyShift action_51
action_21 _ = happyFail

action_22 _ = happyReduce_22

action_23 (13) = happyShift action_2
action_23 (14) = happyShift action_12
action_23 (15) = happyShift action_13
action_23 (16) = happyShift action_14
action_23 (18) = happyShift action_15
action_23 (19) = happyShift action_16
action_23 (21) = happyShift action_17
action_23 (25) = happyShift action_18
action_23 (35) = happyShift action_19
action_23 (4) = happyGoto action_50
action_23 (5) = happyGoto action_4
action_23 (6) = happyGoto action_5
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 (9) = happyGoto action_8
action_23 (10) = happyGoto action_9
action_23 (11) = happyGoto action_10
action_23 (12) = happyGoto action_11
action_23 _ = happyFail

action_24 (14) = happyShift action_12
action_24 (15) = happyShift action_13
action_24 (18) = happyShift action_15
action_24 (19) = happyShift action_16
action_24 (21) = happyShift action_17
action_24 (25) = happyShift action_18
action_24 (35) = happyShift action_19
action_24 (10) = happyGoto action_49
action_24 (11) = happyGoto action_10
action_24 (12) = happyGoto action_11
action_24 _ = happyFail

action_25 (14) = happyShift action_12
action_25 (15) = happyShift action_13
action_25 (18) = happyShift action_15
action_25 (19) = happyShift action_16
action_25 (21) = happyShift action_17
action_25 (25) = happyShift action_18
action_25 (35) = happyShift action_19
action_25 (10) = happyGoto action_48
action_25 (11) = happyGoto action_10
action_25 (12) = happyGoto action_11
action_25 _ = happyFail

action_26 (14) = happyShift action_12
action_26 (15) = happyShift action_13
action_26 (18) = happyShift action_15
action_26 (19) = happyShift action_16
action_26 (21) = happyShift action_17
action_26 (25) = happyShift action_18
action_26 (35) = happyShift action_19
action_26 (10) = happyGoto action_47
action_26 (11) = happyGoto action_10
action_26 (12) = happyGoto action_11
action_26 _ = happyFail

action_27 (14) = happyShift action_12
action_27 (15) = happyShift action_13
action_27 (18) = happyShift action_15
action_27 (19) = happyShift action_16
action_27 (21) = happyShift action_17
action_27 (25) = happyShift action_18
action_27 (35) = happyShift action_19
action_27 (9) = happyGoto action_46
action_27 (10) = happyGoto action_9
action_27 (11) = happyGoto action_10
action_27 (12) = happyGoto action_11
action_27 _ = happyFail

action_28 (14) = happyShift action_12
action_28 (15) = happyShift action_13
action_28 (18) = happyShift action_15
action_28 (19) = happyShift action_16
action_28 (21) = happyShift action_17
action_28 (25) = happyShift action_18
action_28 (35) = happyShift action_19
action_28 (9) = happyGoto action_45
action_28 (10) = happyGoto action_9
action_28 (11) = happyGoto action_10
action_28 (12) = happyGoto action_11
action_28 _ = happyFail

action_29 (14) = happyShift action_12
action_29 (15) = happyShift action_13
action_29 (18) = happyShift action_15
action_29 (19) = happyShift action_16
action_29 (21) = happyShift action_17
action_29 (25) = happyShift action_18
action_29 (35) = happyShift action_19
action_29 (8) = happyGoto action_44
action_29 (9) = happyGoto action_8
action_29 (10) = happyGoto action_9
action_29 (11) = happyGoto action_10
action_29 (12) = happyGoto action_11
action_29 _ = happyFail

action_30 (14) = happyShift action_12
action_30 (15) = happyShift action_13
action_30 (18) = happyShift action_15
action_30 (19) = happyShift action_16
action_30 (21) = happyShift action_17
action_30 (25) = happyShift action_18
action_30 (35) = happyShift action_19
action_30 (8) = happyGoto action_43
action_30 (9) = happyGoto action_8
action_30 (10) = happyGoto action_9
action_30 (11) = happyGoto action_10
action_30 (12) = happyGoto action_11
action_30 _ = happyFail

action_31 (14) = happyShift action_12
action_31 (15) = happyShift action_13
action_31 (18) = happyShift action_15
action_31 (19) = happyShift action_16
action_31 (21) = happyShift action_17
action_31 (25) = happyShift action_18
action_31 (35) = happyShift action_19
action_31 (8) = happyGoto action_42
action_31 (9) = happyGoto action_8
action_31 (10) = happyGoto action_9
action_31 (11) = happyGoto action_10
action_31 (12) = happyGoto action_11
action_31 _ = happyFail

action_32 (14) = happyShift action_12
action_32 (15) = happyShift action_13
action_32 (18) = happyShift action_15
action_32 (19) = happyShift action_16
action_32 (21) = happyShift action_17
action_32 (25) = happyShift action_18
action_32 (35) = happyShift action_19
action_32 (8) = happyGoto action_41
action_32 (9) = happyGoto action_8
action_32 (10) = happyGoto action_9
action_32 (11) = happyGoto action_10
action_32 (12) = happyGoto action_11
action_32 _ = happyFail

action_33 (14) = happyShift action_12
action_33 (15) = happyShift action_13
action_33 (18) = happyShift action_15
action_33 (19) = happyShift action_16
action_33 (21) = happyShift action_17
action_33 (25) = happyShift action_18
action_33 (35) = happyShift action_19
action_33 (8) = happyGoto action_40
action_33 (9) = happyGoto action_8
action_33 (10) = happyGoto action_9
action_33 (11) = happyGoto action_10
action_33 (12) = happyGoto action_11
action_33 _ = happyFail

action_34 (14) = happyShift action_12
action_34 (15) = happyShift action_13
action_34 (18) = happyShift action_15
action_34 (19) = happyShift action_16
action_34 (21) = happyShift action_17
action_34 (25) = happyShift action_18
action_34 (35) = happyShift action_19
action_34 (7) = happyGoto action_39
action_34 (8) = happyGoto action_7
action_34 (9) = happyGoto action_8
action_34 (10) = happyGoto action_9
action_34 (11) = happyGoto action_10
action_34 (12) = happyGoto action_11
action_34 _ = happyFail

action_35 (14) = happyShift action_12
action_35 (15) = happyShift action_13
action_35 (18) = happyShift action_15
action_35 (19) = happyShift action_16
action_35 (21) = happyShift action_17
action_35 (25) = happyShift action_18
action_35 (35) = happyShift action_19
action_35 (6) = happyGoto action_38
action_35 (7) = happyGoto action_6
action_35 (8) = happyGoto action_7
action_35 (9) = happyGoto action_8
action_35 (10) = happyGoto action_9
action_35 (11) = happyGoto action_10
action_35 (12) = happyGoto action_11
action_35 _ = happyFail

action_36 (28) = happyShift action_37
action_36 _ = happyFail

action_37 (13) = happyShift action_2
action_37 (14) = happyShift action_12
action_37 (15) = happyShift action_13
action_37 (16) = happyShift action_14
action_37 (18) = happyShift action_15
action_37 (19) = happyShift action_16
action_37 (21) = happyShift action_17
action_37 (25) = happyShift action_18
action_37 (35) = happyShift action_19
action_37 (4) = happyGoto action_53
action_37 (5) = happyGoto action_4
action_37 (6) = happyGoto action_5
action_37 (7) = happyGoto action_6
action_37 (8) = happyGoto action_7
action_37 (9) = happyGoto action_8
action_37 (10) = happyGoto action_9
action_37 (11) = happyGoto action_10
action_37 (12) = happyGoto action_11
action_37 _ = happyFail

action_38 (34) = happyShift action_34
action_38 _ = happyReduce_4

action_39 _ = happyReduce_6

action_40 (20) = happyShift action_27
action_40 (21) = happyShift action_28
action_40 _ = happyReduce_8

action_41 (20) = happyShift action_27
action_41 (21) = happyShift action_28
action_41 _ = happyReduce_12

action_42 (20) = happyShift action_27
action_42 (21) = happyShift action_28
action_42 _ = happyReduce_10

action_43 (20) = happyShift action_27
action_43 (21) = happyShift action_28
action_43 _ = happyReduce_11

action_44 (20) = happyShift action_27
action_44 (21) = happyShift action_28
action_44 _ = happyReduce_9

action_45 (22) = happyShift action_25
action_45 (23) = happyShift action_26
action_45 _ = happyReduce_15

action_46 (22) = happyShift action_25
action_46 (23) = happyShift action_26
action_46 _ = happyReduce_14

action_47 _ = happyReduce_18

action_48 _ = happyReduce_17

action_49 _ = happyReduce_20

action_50 (26) = happyShift action_52
action_50 _ = happyFail

action_51 _ = happyReduce_29

action_52 (13) = happyShift action_2
action_52 (14) = happyShift action_12
action_52 (15) = happyShift action_13
action_52 (16) = happyShift action_14
action_52 (18) = happyShift action_15
action_52 (19) = happyShift action_16
action_52 (21) = happyShift action_17
action_52 (25) = happyShift action_18
action_52 (35) = happyShift action_19
action_52 (4) = happyGoto action_55
action_52 (5) = happyGoto action_4
action_52 (6) = happyGoto action_5
action_52 (7) = happyGoto action_6
action_52 (8) = happyGoto action_7
action_52 (9) = happyGoto action_8
action_52 (10) = happyGoto action_9
action_52 (11) = happyGoto action_10
action_52 (12) = happyGoto action_11
action_52 _ = happyFail

action_53 (27) = happyShift action_54
action_53 _ = happyFail

action_54 (13) = happyShift action_2
action_54 (14) = happyShift action_12
action_54 (15) = happyShift action_13
action_54 (16) = happyShift action_14
action_54 (18) = happyShift action_15
action_54 (19) = happyShift action_16
action_54 (21) = happyShift action_17
action_54 (25) = happyShift action_18
action_54 (35) = happyShift action_19
action_54 (4) = happyGoto action_57
action_54 (5) = happyGoto action_4
action_54 (6) = happyGoto action_5
action_54 (7) = happyGoto action_6
action_54 (8) = happyGoto action_7
action_54 (9) = happyGoto action_8
action_54 (10) = happyGoto action_9
action_54 (11) = happyGoto action_10
action_54 (12) = happyGoto action_11
action_54 _ = happyFail

action_55 (27) = happyShift action_56
action_55 _ = happyFail

action_56 (17) = happyShift action_58
action_56 _ = happyFail

action_57 _ = happyReduce_1

action_58 (13) = happyShift action_2
action_58 (14) = happyShift action_12
action_58 (15) = happyShift action_13
action_58 (16) = happyShift action_14
action_58 (18) = happyShift action_15
action_58 (19) = happyShift action_16
action_58 (21) = happyShift action_17
action_58 (25) = happyShift action_18
action_58 (35) = happyShift action_19
action_58 (4) = happyGoto action_59
action_58 (5) = happyGoto action_4
action_58 (6) = happyGoto action_5
action_58 (7) = happyGoto action_6
action_58 (8) = happyGoto action_7
action_58 (9) = happyGoto action_8
action_58 (10) = happyGoto action_9
action_58 (11) = happyGoto action_10
action_58 (12) = happyGoto action_11
action_58 _ = happyFail

action_59 _ = happyReduce_2

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Decl happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 8 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Binary Or happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary And happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary EQ happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary LT happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary GT happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary LE happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary GE happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Binary Add happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Binary Sub happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary Mult happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary Div happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Binary Power happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  10 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  11 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Unary Neg happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  11 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Unary Not happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn12
		 (Literal (IntV happy_var_1)
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn12
		 (Literal (BoolV True)
	)

happyReduce_27 = happySpecReduce_1  12 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn12
		 (Literal (BoolV False)
	)

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn12
		 (Var happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenKeyword "var" -> cont 13;
	TokenIdent happy_dollar_dollar -> cont 14;
	TokenInt happy_dollar_dollar -> cont 15;
	TokenKeyword "if" -> cont 16;
	TokenKeyword "else" -> cont 17;
	TokenKeyword "true" -> cont 18;
	TokenKeyword "false" -> cont 19;
	TokenSymbol "+" -> cont 20;
	TokenSymbol "-" -> cont 21;
	TokenSymbol "*" -> cont 22;
	TokenSymbol "/" -> cont 23;
	TokenSymbol "^" -> cont 24;
	TokenSymbol "(" -> cont 25;
	TokenSymbol ")" -> cont 26;
	TokenSymbol ";" -> cont 27;
	TokenSymbol "=" -> cont 28;
	TokenSymbol "<" -> cont 29;
	TokenSymbol "<=" -> cont 30;
	TokenSymbol ">" -> cont 31;
	TokenSymbol ">=" -> cont 32;
	TokenSymbol "==" -> cont 33;
	TokenSymbol "&&" -> cont 34;
	TokenSymbol "!" -> cont 35;
	TokenSymbol "||" -> cont 36;
	_ -> happyError' (tk:tks)
	}

happyError_ 37 tk tks = happyError' tks
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
      = TokenInt Int
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

        lexNum cs = TokenInt (read num) : lexer' rest
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

symbols = ["{", "}", ",", "+", "-", "*", "/", "(", ")", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!", "^"]
keywords = ["function", "var", "if", "else", "true", "false"]

parseExpr = parser . lexer symbols keywords
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/Users/jeremybi/.stack/programs/x86_64-osx/ghc-7.10.2/lib/ghc-7.10.2/include/ghcversion.h" #-}


















{-# LINE 17 "<built-in>" #-}
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

