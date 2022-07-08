
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

-- export BuiltinT (AddInteger SubtractInteger MultiplyInteger DivideInteger QuotientInteger RemainderInteger ModInteger EqualsInteger LessThanInteger LessThanEqualsInteger AppendByteString ConsByteString SliceByteString LengthByteString IndexByteString EqualsByteString LessThanByteString LessThanEqualByteString Sha2_256 Sha3_256 Blake2b_256 VerifySignature AppendString EqualsString EncodeUtf8 DecodeUtf8 IfThenElse ChooseUnit Trace FstPair SndPair ChooseList MkCons HeadList TailList NullList ChooseData ConstrData MapData ListData IData BData UnConstrData UnMapData UnListData UnBData UnIData EqualsData MkPairData MkNilData MkNilPairData)


end Pluto.Language
