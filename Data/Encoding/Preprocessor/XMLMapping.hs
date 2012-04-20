module Data.Encoding.Preprocessor.XMLMapping where

import Data.Word
import Text.Read
import Text.Show
import Numeric
import Data.List (find)
import Data.Char
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Types

testFile :: FilePath -> IO CharacterMapping
testFile fp = fReadXml fp

{-Type decls-}

data CharacterMapping = CharacterMapping CharacterMapping_Attrs
                                         (Maybe History)
                                         (OneOf2 Validity Stateful_siso)
                                         Assignments
                      deriving (Eq,Show)
data CharacterMapping_Attrs = CharacterMapping_Attrs
    { characterMappingId :: String
    , characterMappingVersion :: String
    , characterMappingDescription :: (Maybe String)
    , characterMappingContact :: (Maybe String)
    , characterMappingRegistrationAuthority :: (Maybe String)
    , characterMappingRegistrationName :: (Maybe String)
    , characterMappingCopyright :: (Maybe String)
    , characterMappingBidiOrder :: (Defaultable CharacterMapping_bidiOrder)
    , characterMappingCombiningOrder :: (Defaultable CharacterMapping_combiningOrder)
    , characterMappingNormalization :: (Defaultable CharacterMapping_normalization)
    } deriving (Eq,Show)
data CharacterMapping_bidiOrder = CharacterMapping_bidiOrder_logical
                                   |  CharacterMapping_bidiOrder_RTL  | 
                                  CharacterMapping_bidiOrder_LTR
                                deriving (Eq,Show)
data CharacterMapping_combiningOrder = CharacterMapping_combiningOrder_before
                                        |  CharacterMapping_combiningOrder_after
                                     deriving (Eq,Show)
data CharacterMapping_normalization = CharacterMapping_normalization_undetermined
                                       |  CharacterMapping_normalization_neither  | 
                                      CharacterMapping_normalization_NFC  | 
                                      CharacterMapping_normalization_NFD  | 
                                      CharacterMapping_normalization_NFC_NFD
                                    deriving (Eq,Show)
data Stateful_siso = Stateful_siso Validity Validity
                   deriving (Eq,Show)
newtype History = History (List1 Modified) 		deriving (Eq,Show)
data Modified = Modified Modified_Attrs String
              deriving (Eq,Show)
data Modified_Attrs = Modified_Attrs
    { modifiedVersion :: String
    , modifiedDate :: String
    } deriving (Eq,Show)
newtype Validity = Validity (List1 State) 		deriving (Eq,Show)
data State = State
    { stateType :: String
    , stateNext :: String
    , stateS :: ByteSequence
    , stateE :: (Maybe ByteSequence)
    , stateMax :: (Maybe String)
    } deriving (Eq,Show)
data Assignments = Assignments Assignments_Attrs [A] [Fub] [Fbu]
                               [Sub1] [Range]
                 deriving (Eq,Show)
data Assignments_Attrs = Assignments_Attrs
    { assignmentsSub :: (Defaultable String)
    , assignmentsSub1 :: (Maybe String)
    } deriving (Eq,Show)
data A = A
    { aB :: ByteSequence
    , aU :: CodePoints
    , aC :: (Maybe String)
    , aV :: (Maybe String)
    } deriving (Eq,Show)
data Fub = Fub
    { fubB :: ByteSequence
    , fubU :: CodePoints
    , fubC :: (Maybe String)
    , fubRu :: (Maybe String)
    , fubRc :: (Maybe String)
    , fubV :: (Maybe String)
    } deriving (Eq,Show)
data Fbu = Fbu
    { fbuB :: ByteSequence
    , fbuU :: CodePoints
    , fbuV :: (Maybe String)
    } deriving (Eq,Show)
data Sub1 = Sub1
    { sub1U :: CodePoints
    , sub1C :: (Maybe String)
    , sub1V :: (Maybe String)
    } deriving (Eq,Show)
data Range = Range
    { rangeBFirst :: ByteSequence
    , rangeBLast :: ByteSequence
    , rangeUFirst :: CodePoints
    , rangeULast :: CodePoints
    , rangeBMin :: ByteSequence
    , rangeBMax :: ByteSequence
    , rangeV :: (Maybe String)
    } deriving (Eq,Show)
data Iso2022 = Iso2022 (Maybe Default2022)
                       (List1 (OneOf5 Escape Si So Ss2 Ss3))
             deriving (Eq,Show)
data Default2022 = Default2022
    { default2022Name :: String
    } deriving (Eq,Show)
data Escape = Escape
    { escapeSequence :: String
    , escapeName :: String
    } deriving (Eq,Show)
newtype Si = Si (List1 Designator) 		deriving (Eq,Show)
newtype So = So (List1 Designator) 		deriving (Eq,Show)
newtype Ss2 = Ss2 (List1 Designator) 		deriving (Eq,Show)
newtype Ss3 = Ss3 (List1 Designator) 		deriving (Eq,Show)
data Designator = Designator
    { designatorSequence :: String
    , designatorName :: String
    } deriving (Eq,Show)

newtype ByteSequence = BS [Word8] deriving Eq

newtype CodePoints = CP [Char] deriving Eq

{-Instance decls-}

instance HTypeable CharacterMapping where
    toHType x = Defined "characterMapping" [] []
instance XmlContent CharacterMapping where
    toContents (CharacterMapping as a b c) =
        [CElem (Elem (N "characterMapping") (toAttrs as) (maybe [] toContents a
                                                      ++ toContents b
                                                      ++ toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["characterMapping"]
        ; interior e $ return (CharacterMapping (fromAttrs as))
                       `apply` optional parseContents `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <characterMapping>, "++)
instance XmlAttributes CharacterMapping_Attrs where
    fromAttrs as =
        CharacterMapping_Attrs
          { characterMappingId = definiteA fromAttrToStr "characterMapping" "id" as
          , characterMappingVersion = definiteA fromAttrToStr "characterMapping" "version" as
          , characterMappingDescription = possibleA fromAttrToStr "description" as
          , characterMappingContact = possibleA fromAttrToStr "contact" as
          , characterMappingRegistrationAuthority = possibleA fromAttrToStr "registrationAuthority" as
          , characterMappingRegistrationName = possibleA fromAttrToStr "registrationName" as
          , characterMappingCopyright = possibleA fromAttrToStr "copyright" as
          , characterMappingBidiOrder = defaultA fromAttrToTyp CharacterMapping_bidiOrder_logical "bidiOrder" as
          , characterMappingCombiningOrder = defaultA fromAttrToTyp CharacterMapping_combiningOrder_after "combiningOrder" as
          , characterMappingNormalization = defaultA fromAttrToTyp CharacterMapping_normalization_undetermined "normalization" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (characterMappingId v)
        , toAttrFrStr "version" (characterMappingVersion v)
        , maybeToAttr toAttrFrStr "description" (characterMappingDescription v)
        , maybeToAttr toAttrFrStr "contact" (characterMappingContact v)
        , maybeToAttr toAttrFrStr "registrationAuthority" (characterMappingRegistrationAuthority v)
        , maybeToAttr toAttrFrStr "registrationName" (characterMappingRegistrationName v)
        , maybeToAttr toAttrFrStr "copyright" (characterMappingCopyright v)
        , defaultToAttr toAttrFrTyp "bidiOrder" (characterMappingBidiOrder v)
        , defaultToAttr toAttrFrTyp "combiningOrder" (characterMappingCombiningOrder v)
        , defaultToAttr toAttrFrTyp "normalization" (characterMappingNormalization v)
        ]

instance XmlAttrType CharacterMapping_bidiOrder where
    fromAttrToTyp n (n',v)
        | N n==n'   = translate (attr2str v)
        | otherwise = Nothing
      where translate "logical" = Just CharacterMapping_bidiOrder_logical
            translate "RTL" = Just CharacterMapping_bidiOrder_RTL
            translate "LTR" = Just CharacterMapping_bidiOrder_LTR
            translate _ = Nothing
    toAttrFrTyp n CharacterMapping_bidiOrder_logical = Just (N n, str2attr "logical")
    toAttrFrTyp n CharacterMapping_bidiOrder_RTL = Just (N n, str2attr "RTL")
    toAttrFrTyp n CharacterMapping_bidiOrder_LTR = Just (N n, str2attr "LTR")

instance XmlAttrType CharacterMapping_combiningOrder where
    fromAttrToTyp n (n',v)
        | N n==n'   = translate (attr2str v)
        | otherwise = Nothing
      where translate "before" = Just CharacterMapping_combiningOrder_before
            translate "after" = Just CharacterMapping_combiningOrder_after
            translate _ = Nothing
    toAttrFrTyp n CharacterMapping_combiningOrder_before = Just (N n, str2attr "before")
    toAttrFrTyp n CharacterMapping_combiningOrder_after = Just (N n, str2attr "after")

instance XmlAttrType CharacterMapping_normalization where
    fromAttrToTyp n (n',v)
        | N n==n'   = translate (attr2str v)
        | otherwise = Nothing
      where translate "undetermined" = Just CharacterMapping_normalization_undetermined
            translate "neither" = Just CharacterMapping_normalization_neither
            translate "NFC" = Just CharacterMapping_normalization_NFC
            translate "NFD" = Just CharacterMapping_normalization_NFD
            translate "NFC_NFD" = Just CharacterMapping_normalization_NFC_NFD
            translate _ = Nothing
    toAttrFrTyp n CharacterMapping_normalization_undetermined = Just (N n, str2attr "undetermined")
    toAttrFrTyp n CharacterMapping_normalization_neither = Just (N n, str2attr "neither")
    toAttrFrTyp n CharacterMapping_normalization_NFC = Just (N n, str2attr "NFC")
    toAttrFrTyp n CharacterMapping_normalization_NFD = Just (N n, str2attr "NFD")
    toAttrFrTyp n CharacterMapping_normalization_NFC_NFD = Just (N n, str2attr "NFC_NFD")

instance XmlAttrType ByteSequence where
    fromAttrToTyp n (n',v)
        | N n==n' = parseByteSequence (attr2str v)
        | otherwise = Nothing
    toAttrFrTyp n bs = Just (N n, str2attr $ show bs)

parseByteSequence :: String -> Maybe ByteSequence
parseByteSequence str = do
  seq <- mapM (\w -> do
                (res,_) <- find (null.snd) (readHex w)
                return res
             ) (words str)
  return $ BS seq

instance Show ByteSequence where
    show (BS seq) = foldl (\f w -> f . (showChar ' ') . (showHex w)) id seq ""

instance XmlAttrType CodePoints where
    fromAttrToTyp n (n',v)
        | N n==n' = parseCodePoints (attr2str v)
        | otherwise = Nothing
    toAttrFrTyp n bs = Just (N n, str2attr $ show bs)

parseCodePoints :: String -> Maybe CodePoints
parseCodePoints str = do
  seq <- mapM (\w -> do
                (res,_) <- find (null.snd) (readHex w)
                return (chr res)
             ) (words str)
  return $ CP seq

instance Show CodePoints where
    show (CP seq) = foldl (\f w -> f . (showChar ' ') . (showHex (ord w))) id seq ""

instance HTypeable Stateful_siso where
    toHType x = Defined "stateful_siso" [] []
instance XmlContent Stateful_siso where
    toContents (Stateful_siso a b) =
        [CElem (Elem (N "stateful_siso") [] (toContents a ++ toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["stateful_siso"]
        ; interior e $ return (Stateful_siso) `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <stateful_siso>, "++)

instance HTypeable History where
    toHType x = Defined "history" [] []
instance XmlContent History where
    toContents (History a) =
        [CElem (Elem (N "history") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["history"]
        ; interior e $ return (History) `apply` parseContents
        } `adjustErr` ("in <history>, "++)

instance HTypeable Modified where
    toHType x = Defined "modified" [] []
instance XmlContent Modified where
    toContents (Modified as a) =
        [CElem (Elem (N "modified") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["modified"]
        ; interior e $ return (Modified (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <modified>, "++)
instance XmlAttributes Modified_Attrs where
    fromAttrs as =
        Modified_Attrs
          { modifiedVersion = definiteA fromAttrToStr "modified" "version" as
          , modifiedDate = definiteA fromAttrToStr "modified" "date" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "version" (modifiedVersion v)
        , toAttrFrStr "date" (modifiedDate v)
        ]

instance HTypeable Validity where
    toHType x = Defined "validity" [] []
instance XmlContent Validity where
    toContents (Validity a) =
        [CElem (Elem (N "validity") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["validity"]
        ; interior e $ return (Validity) `apply` parseContents
        } `adjustErr` ("in <validity>, "++)

instance HTypeable State where
    toHType x = Defined "state" [] []
instance XmlContent State where
    toContents as =
        [CElem (Elem (N "state") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["state"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <state>, "++)
instance XmlAttributes State where
    fromAttrs as =
        State
          { stateType = definiteA fromAttrToStr "state" "type" as
          , stateNext = definiteA fromAttrToStr "state" "next" as
          , stateS = definiteA fromAttrToTyp "state" "s" as
          , stateE = possibleA fromAttrToTyp "e" as
          , stateMax = possibleA fromAttrToStr "max" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "type" (stateType v)
        , toAttrFrStr "next" (stateNext v)
        , toAttrFrTyp "s" (stateS v)
        , maybeToAttr toAttrFrTyp "e" (stateE v)
        , maybeToAttr toAttrFrStr "max" (stateMax v)
        ]

instance HTypeable Assignments where
    toHType x = Defined "assignments" [] []
instance XmlContent Assignments where
    toContents (Assignments as a b c d e) =
        [CElem (Elem (N "assignments") (toAttrs as) (concatMap toContents a ++
                                                 concatMap toContents b ++ concatMap toContents c ++
                                                 concatMap toContents d ++
                                                 concatMap toContents e)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["assignments"]
        ; interior e $ return (Assignments (fromAttrs as))
                       `apply` many parseContents `apply` many parseContents
                       `apply` many parseContents `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <assignments>, "++)
instance XmlAttributes Assignments_Attrs where
    fromAttrs as =
        Assignments_Attrs
          { assignmentsSub = defaultA fromAttrToStr "1A" "sub" as
          , assignmentsSub1 = possibleA fromAttrToStr "sub1" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "sub" (assignmentsSub v)
        , maybeToAttr toAttrFrStr "sub1" (assignmentsSub1 v)
        ]

instance HTypeable A where
    toHType x = Defined "a" [] []
instance XmlContent A where
    toContents as =
        [CElem (Elem (N "a") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["a"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <a>, "++)
instance XmlAttributes A where
    fromAttrs as =
        A { aB = definiteA fromAttrToTyp "a" "b" as
          , aU = definiteA fromAttrToTyp "a" "u" as
          , aC = possibleA fromAttrToStr "c" as
          , aV = possibleA fromAttrToStr "v" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "b" (aB v)
        , toAttrFrTyp "u" (aU v)
        , maybeToAttr toAttrFrStr "c" (aC v)
        , maybeToAttr toAttrFrStr "v" (aV v)
        ]

instance HTypeable Fub where
    toHType x = Defined "fub" [] []
instance XmlContent Fub where
    toContents as =
        [CElem (Elem (N "fub") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["fub"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <fub>, "++)
instance XmlAttributes Fub where
    fromAttrs as =
        Fub
          { fubB = definiteA fromAttrToTyp "fub" "b" as
          , fubU = definiteA fromAttrToTyp "fub" "u" as
          , fubC = possibleA fromAttrToStr "c" as
          , fubRu = possibleA fromAttrToStr "ru" as
          , fubRc = possibleA fromAttrToStr "rc" as
          , fubV = possibleA fromAttrToStr "v" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "b" (fubB v)
        , toAttrFrTyp "u" (fubU v)
        , maybeToAttr toAttrFrStr "c" (fubC v)
        , maybeToAttr toAttrFrStr "ru" (fubRu v)
        , maybeToAttr toAttrFrStr "rc" (fubRc v)
        , maybeToAttr toAttrFrStr "v" (fubV v)
        ]

instance HTypeable Fbu where
    toHType x = Defined "fbu" [] []
instance XmlContent Fbu where
    toContents as =
        [CElem (Elem (N "fbu") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["fbu"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <fbu>, "++)
instance XmlAttributes Fbu where
    fromAttrs as =
        Fbu
          { fbuB = definiteA fromAttrToTyp "fbu" "b" as
          , fbuU = definiteA fromAttrToTyp "fbu" "u" as
          , fbuV = possibleA fromAttrToStr "v" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "b" (fbuB v)
        , toAttrFrTyp "u" (fbuU v)
        , maybeToAttr toAttrFrStr "v" (fbuV v)
        ]

instance HTypeable Sub1 where
    toHType x = Defined "sub1" [] []
instance XmlContent Sub1 where
    toContents as =
        [CElem (Elem (N "sub1") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["sub1"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <sub1>, "++)
instance XmlAttributes Sub1 where
    fromAttrs as =
        Sub1
          { sub1U = definiteA fromAttrToTyp "sub1" "u" as
          , sub1C = possibleA fromAttrToStr "c" as
          , sub1V = possibleA fromAttrToStr "v" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "u" (sub1U v)
        , maybeToAttr toAttrFrStr "c" (sub1C v)
        , maybeToAttr toAttrFrStr "v" (sub1V v)
        ]

instance HTypeable Range where
    toHType x = Defined "range" [] []
instance XmlContent Range where
    toContents as =
        [CElem (Elem (N "range") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["range"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <range>, "++)
instance XmlAttributes Range where
    fromAttrs as =
        Range
          { rangeBFirst = definiteA fromAttrToTyp "range" "bFirst" as
          , rangeBLast = definiteA fromAttrToTyp "range" "bLast" as
          , rangeUFirst = definiteA fromAttrToTyp "range" "uFirst" as
          , rangeULast = definiteA fromAttrToTyp "range" "uLast" as
          , rangeBMin = definiteA fromAttrToTyp "range" "bMin" as
          , rangeBMax = definiteA fromAttrToTyp "range" "bMax" as
          , rangeV = possibleA fromAttrToStr "v" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "bFirst" (rangeBFirst v)
        , toAttrFrTyp "bLast" (rangeBLast v)
        , toAttrFrTyp "uFirst" (rangeUFirst v)
        , toAttrFrTyp "uLast" (rangeULast v)
        , toAttrFrTyp "bMin" (rangeBMin v)
        , toAttrFrTyp "bMax" (rangeBMax v)
        , maybeToAttr toAttrFrStr "v" (rangeV v)
        ]

instance HTypeable Iso2022 where
    toHType x = Defined "iso2022" [] []
instance XmlContent Iso2022 where
    toContents (Iso2022 a b) =
        [CElem (Elem (N "iso2022") [] (maybe [] toContents a ++
                                   toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["iso2022"]
        ; interior e $ return (Iso2022) `apply` optional parseContents
                       `apply` parseContents
        } `adjustErr` ("in <iso2022>, "++)

instance HTypeable Default2022 where
    toHType x = Defined "default2022" [] []
instance XmlContent Default2022 where
    toContents as =
        [CElem (Elem (N "default2022") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["default2022"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <default2022>, "++)
instance XmlAttributes Default2022 where
    fromAttrs as =
        Default2022
          { default2022Name = definiteA fromAttrToStr "default2022" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (default2022Name v)
        ]

instance HTypeable Escape where
    toHType x = Defined "escape" [] []
instance XmlContent Escape where
    toContents as =
        [CElem (Elem (N "escape") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["escape"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <escape>, "++)
instance XmlAttributes Escape where
    fromAttrs as =
        Escape
          { escapeSequence = definiteA fromAttrToStr "escape" "sequence" as
          , escapeName = definiteA fromAttrToStr "escape" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "sequence" (escapeSequence v)
        , toAttrFrStr "name" (escapeName v)
        ]

instance HTypeable Si where
    toHType x = Defined "si" [] []
instance XmlContent Si where
    toContents (Si a) =
        [CElem (Elem (N "si") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["si"]
        ; interior e $ return (Si) `apply` parseContents
        } `adjustErr` ("in <si>, "++)

instance HTypeable So where
    toHType x = Defined "so" [] []
instance XmlContent So where
    toContents (So a) =
        [CElem (Elem (N "so") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["so"]
        ; interior e $ return (So) `apply` parseContents
        } `adjustErr` ("in <so>, "++)

instance HTypeable Ss2 where
    toHType x = Defined "ss2" [] []
instance XmlContent Ss2 where
    toContents (Ss2 a) =
        [CElem (Elem (N "ss2") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ss2"]
        ; interior e $ return (Ss2) `apply` parseContents
        } `adjustErr` ("in <ss2>, "++)

instance HTypeable Ss3 where
    toHType x = Defined "ss3" [] []
instance XmlContent Ss3 where
    toContents (Ss3 a) =
        [CElem (Elem (N "ss3") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ss3"]
        ; interior e $ return (Ss3) `apply` parseContents
        } `adjustErr` ("in <ss3>, "++)

instance HTypeable Designator where
    toHType x = Defined "designator" [] []
instance XmlContent Designator where
    toContents as =
        [CElem (Elem (N "designator") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["designator"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <designator>, "++)
instance XmlAttributes Designator where
    fromAttrs as =
        Designator
          { designatorSequence = definiteA fromAttrToStr "designator" "sequence" as
          , designatorName = definiteA fromAttrToStr "designator" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "sequence" (designatorSequence v)
        , toAttrFrStr "name" (designatorName v)
        ]



{-Done-}
