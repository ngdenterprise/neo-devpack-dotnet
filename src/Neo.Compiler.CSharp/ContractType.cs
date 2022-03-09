using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using System;

namespace Neo.Compiler
{
    enum PrimitiveType : byte
    {
        Boolean,
        Integer,
        ByteArray,
        String,
        Hash160,
        Hash256,
        PublicKey,
        Signature,
        Address,
    }

    abstract record ContractType();

    record ArrayContractType(ContractType Type) : ContractType;
    record InteropContractType(StructRefContractType StructRef) : ContractType;
    record MapContractType(PrimitiveType KeyType, ContractType ValueType) : ContractType;

    record PrimitiveContractType(PrimitiveType Type) : ContractType
    {
        public readonly static PrimitiveContractType Address = new PrimitiveContractType(PrimitiveType.Address);
        public readonly static PrimitiveContractType Boolean = new PrimitiveContractType(PrimitiveType.Boolean);
        public readonly static PrimitiveContractType ByteArray = new PrimitiveContractType(PrimitiveType.ByteArray);
        public readonly static PrimitiveContractType Hash160 = new PrimitiveContractType(PrimitiveType.Hash160);
        public readonly static PrimitiveContractType Hash256 = new PrimitiveContractType(PrimitiveType.Hash256);
        public readonly static PrimitiveContractType Integer = new PrimitiveContractType(PrimitiveType.Integer);
        public readonly static PrimitiveContractType PublicKey = new PrimitiveContractType(PrimitiveType.PublicKey);
        public readonly static PrimitiveContractType Signature = new PrimitiveContractType(PrimitiveType.Signature);
        public readonly static PrimitiveContractType String = new PrimitiveContractType(PrimitiveType.String);
    }
    record StructRefContractType(string Name, string Namespace) : ContractType;

    record UnspecifiedContractType() : ContractType
    {
        public readonly static UnspecifiedContractType Unspecified = new UnspecifiedContractType();
    }

    static class ContractTypeExtensions
    {
        public static string AsEncodedType(this ISymbol @this, ContractTypeVisitor resolver) => resolver.Resolve(@this).AsEncodedType();

        public static string AsEncodedType(this ContractType type)
        {
            return type switch
            {
                ArrayContractType a => $"#Array<{a.Type.AsEncodedType()}>",
                InteropContractType i => $"#Interop<{i.StructRef.AsEncodedType()}>",
                MapContractType m => $"#Map<#{m.KeyType}:{m.ValueType.AsEncodedType()}>",
                PrimitiveContractType p => $"#{p.Type}",
                StructRefContractType s => $"{s.Namespace}.{s.Name}",
                UnspecifiedContractType => "#Unspecified",
                _ => throw new NotImplementedException($"{nameof(AsEncodedType)} {type.GetType().Name}"),
            };
        }
    }
}
