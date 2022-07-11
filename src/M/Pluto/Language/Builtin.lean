

namespace Pluto.Language


inductive BuiltinT where
  | AddInteger
  | SubtractInteger
  | MultiplyInteger
  | DivideInteger
  | QuotientInteger
  | RemainderInteger
  | ModInteger
  | EqualsInteger
  | LessThanInteger
  | LessThanEqualsInteger
  | AppendByteString
  | ConsByteString
  | SliceByteString
  | LengthByteString
  | IndexByteString
  | EqualsByteString
  | LessThanByteString
  | LessThanEqualByteString
  | Sha2_256
  | Sha3_256
  | Blake2b_256
  | VerifySignature
  | AppendString
  | EqualsString
  | EncodeUtf8
  | DecodeUtf8
  | IfThenElse
  | ChooseUnit
  | Trace
  | FstPair
  | SndPair
  | ChooseList
  | MkCons
  | HeadList
  | TailList
  | NullList
  | ChooseData
  | ConstrData
  | MapData
  | ListData
  | IData
  | BData
  | UnConstrData
  | UnMapData
  | UnListData
  | UnBData
  | UnIData
  | EqualsData
  | MkPairData
  | MkNilData
  | MkNilPairData
deriving Repr

export BuiltinT (AddInteger SubtractInteger MultiplyInteger DivideInteger QuotientInteger RemainderInteger ModInteger EqualsInteger LessThanInteger LessThanEqualsInteger AppendByteString ConsByteString SliceByteString LengthByteString IndexByteString EqualsByteString LessThanByteString LessThanEqualByteString Sha2_256 Sha3_256 Blake2b_256 VerifySignature AppendString EqualsString EncodeUtf8 DecodeUtf8 /- IfThenElse -/ ChooseUnit Trace FstPair SndPair ChooseList MkCons HeadList TailList NullList ChooseData ConstrData MapData ListData IData BData UnConstrData UnMapData UnListData UnBData UnIData EqualsData MkPairData MkNilData MkNilPairData)


instance : ToString BuiltinT where
  toString : BuiltinT → String
    | AddInteger              => "AddInteger"
    | SubtractInteger         => "SubtractInteger"
    | MultiplyInteger         => "MultiplyInteger"
    | DivideInteger           => "DivideInteger"
    | QuotientInteger         => "QuotientInteger"
    | RemainderInteger        => "RemainderInteger"
    | ModInteger              => "ModInteger"
    | EqualsInteger           => "EqualsInteger"
    | LessThanInteger         => "LessThanInteger"
    | LessThanEqualsInteger   => "LessThanEqualsInteger"
    | AppendByteString        => "AppendByteString"
    | ConsByteString          => "ConsByteString"
    | SliceByteString         => "SliceByteString"
    | LengthByteString        => "LengthByteString"
    | IndexByteString         => "IndexByteString"
    | EqualsByteString        => "EqualsByteString"
    | LessThanByteString      => "LessThanByteString"
    | LessThanEqualByteString => "LessThanEqualByteString"
    | Sha2_256                => "Sha2_256"
    | Sha3_256                => "Sha3_256"
    | Blake2b_256             => "Blake2b_256"
    | VerifySignature         => "VerifySignature"
    | AppendString            => "AppendString"
    | EqualsString            => "EqualsString"
    | EncodeUtf8              => "EncodeUtf8"
    | DecodeUtf8              => "DecodeUtf8"
    | BuiltinT.IfThenElse     => "IfThenElse"
    | ChooseUnit              => "ChooseUnit"
    | Trace                   => "Trace"
    | FstPair                 => "FstPair"
    | SndPair                 => "SndPair"
    | ChooseList              => "ChooseList"
    | MkCons                  => "MkCons"
    | HeadList                => "HeadList"
    | TailList                => "TailList"
    | NullList                => "NullList"
    | ChooseData              => "ChooseData"
    | ConstrData              => "ConstrData"
    | MapData                 => "MapData"
    | ListData                => "ListData"
    | IData                   => "IData"
    | BData                   => "BData"
    | UnConstrData            => "UnConstrData"
    | UnMapData               => "UnMapData"
    | UnListData              => "UnListData"
    | UnBData                 => "UnBData"
    | UnIData                 => "UnIData"
    | EqualsData              => "EqualsData"
    | MkPairData              => "MkPairData"
    | MkNilData               => "MkNilData"
    | MkNilPairData           => "MkNilPairData"


end Pluto.Language
