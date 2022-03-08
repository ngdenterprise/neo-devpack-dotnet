using Microsoft.CodeAnalysis;
using System;
using System.Linq;

namespace Neo.Compiler
{
    class ContractTypeVisitor : SymbolVisitor<ContractType>
    {
        readonly TypeCache typeCache;
        public ContractTypeVisitor(TypeCache typeCache)
        {
            this.typeCache = typeCache;
        }

        public ContractType Resolve(ISymbol symbol) => Visit(symbol) ?? UnspecifiedContractType.Unspecified;

        public override ContractType? VisitNamedType(INamedTypeSymbol symbol)
        {
            if (symbol.TypeKind == TypeKind.Enum) return PrimitiveContractType.Integer;

            return symbol.SpecialType switch
            {
                SpecialType.System_Boolean => PrimitiveContractType.Boolean,
                SpecialType.System_String => PrimitiveContractType.String,
                SpecialType.System_Void => VoidContractType.Void,
                SpecialType.System_Char => PrimitiveContractType.Integer,
                SpecialType.System_Byte => PrimitiveContractType.Integer,
                SpecialType.System_SByte => PrimitiveContractType.Integer,
                SpecialType.System_Int16 => PrimitiveContractType.Integer,
                SpecialType.System_Int32 => PrimitiveContractType.Integer,
                SpecialType.System_Int64 => PrimitiveContractType.Integer,
                SpecialType.System_UInt16 => PrimitiveContractType.Integer,
                SpecialType.System_UInt32 => PrimitiveContractType.Integer,
                SpecialType.System_UInt64 => PrimitiveContractType.Integer,
                SpecialType.System_Object => UnspecifiedContractType.Unspecified,
                SpecialType.None => ConvertSymbol(symbol),
                _ => throw new NotSupportedException($"SpecialType {symbol.SpecialType}")
            }; 
        }

        public override ContractType? VisitArrayType(IArrayTypeSymbol symbol)
        {
            if (symbol.ElementType.SpecialType == SpecialType.System_Byte)
                return PrimitiveContractType.ByteArray;

            var elementType = Resolve(symbol.ElementType);
            return new ArrayContractType(elementType);
        }

        ContractType? ConvertSymbol(INamedTypeSymbol symbol)
        {
            // if (equals(symbol, address)) return PrimitiveContractType.Address;
            if (typeCache.Equals(symbol, "System.Numerics.BigInteger")) return PrimitiveContractType.Integer;
            if (typeCache.Equals(symbol, "Neo.SmartContract.Framework.ByteString")) return PrimitiveContractType.ByteArray;
            if (typeCache.Equals(symbol, "Neo.Cryptography.ECC.ECPoint")) return PrimitiveContractType.PublicKey;
            if (typeCache.Equals(symbol, "Neo.UInt160")) return PrimitiveContractType.Hash160;
            if (typeCache.Equals(symbol, "Neo.UInt256")) return PrimitiveContractType.Hash256;

            if (symbol.AllInterfaces.Any(i => typeCache.Equals(i, "Neo.SmartContract.Framework.IApiInterface")))
                return new InteropContractType(symbol);

            if (symbol.IsGenericType)
            {
                if (typeCache.Equals(symbol.ConstructedFrom, "Neo.SmartContract.Framework.Map`2"))
                {
                    var key = Resolve(symbol.TypeArguments[0]) as PrimitiveContractType;
                    if (key is null) throw new Exception("Invalid Map Key Type");

                    var value = Resolve(symbol.TypeArguments[1]);
                    return new MapContractType(key.Type, value);
                }

                if (typeCache.Equals(symbol.ConstructedFrom, "Neo.SmartContract.Framework.List`1"))
                {
                    var type = Resolve(symbol.TypeArguments[0]);
                    return new ArrayContractType(type);
                }

                throw new NotSupportedException($"Only List<T> and Map<K,V> concrete generic types are supported");
            }

            return SymbolEqualityComparer.Default.Equals(symbol.ContainingAssembly, typeCache.FindType("Neo.UInt160").ContainingAssembly)
                ? new NeoScfxContractType(symbol)
                : new SymbolContractType(symbol);
        }
    }
}
