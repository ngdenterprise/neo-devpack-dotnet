using Microsoft.CodeAnalysis;
using System;
using System.Linq;

namespace Neo.Compiler
{
    class ContractTypeVisitor : SymbolVisitor<ContractType>
    {
        // readonly INamedTypeSymbol? address;
        readonly INamedTypeSymbol apiInterface;
        readonly INamedTypeSymbol bigInt;
        readonly INamedTypeSymbol byteString;
        readonly INamedTypeSymbol ecPoint;
        readonly INamedTypeSymbol list;
        readonly INamedTypeSymbol map;
        public readonly INamedTypeSymbol SmartContract;
        readonly INamedTypeSymbol uint160;
        readonly INamedTypeSymbol uint256;
        readonly IAssemblySymbol scfx;

        public ContractTypeVisitor(Compilation compilation)
        {
            // address = compilation.GetTypeByMetadataName("Neo.SmartContract.Framework.Address");
            apiInterface = compilation.FindType("Neo.SmartContract.Framework.IApiInterface");
            bigInt = compilation.FindType("System.Numerics.BigInteger");
            byteString = compilation.FindType("Neo.SmartContract.Framework.ByteString");
            ecPoint = compilation.FindType("Neo.Cryptography.ECC.ECPoint");
            list = compilation.FindType("Neo.SmartContract.Framework.List`1");
            map = compilation.FindType("Neo.SmartContract.Framework.Map`2");
            SmartContract = compilation.FindType("Neo.SmartContract.Framework.SmartContract");
            uint160 = compilation.FindType("Neo.UInt160");
            uint256 = compilation.FindType("Neo.UInt256");

            scfx = uint160.ContainingAssembly;
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
            Func<ISymbol?, ISymbol?, bool> equals = SymbolEqualityComparer.Default.Equals;
            // if (equals(symbol, address)) return PrimitiveContractType.Address;
            if (equals(symbol, bigInt)) return PrimitiveContractType.Integer;
            if (equals(symbol, byteString)) return PrimitiveContractType.ByteArray;
            if (equals(symbol, ecPoint)) return PrimitiveContractType.PublicKey;
            if (equals(symbol, uint160)) return PrimitiveContractType.Hash160;
            if (equals(symbol, uint256)) return PrimitiveContractType.Hash256;

            if (symbol.AllInterfaces.Any(i => equals(i, apiInterface)))
                return new InteropContractType(symbol);

            if (symbol.IsGenericType)
            {
                if (equals(map, symbol.ConstructedFrom))
                {
                    var key = Resolve(symbol.TypeArguments[0]) as PrimitiveContractType;
                    if (key is null) throw new Exception("Invalid Map Key Type");

                    var value = Resolve(symbol.TypeArguments[1]);
                    return new MapContractType(key.Type, value);
                }

                if (equals(map, symbol.ConstructedFrom))
                {
                    var type = Resolve(symbol.TypeArguments[0]);
                    return new ArrayContractType(type);
                }

                throw new NotSupportedException($"Only List<T> and Map<K,V> concrete generic types are supported");
            }

            return equals(symbol.ContainingAssembly, scfx)
                ? new NeoScfxContractType(symbol)
                : new SymbolContractType(symbol);
        }
    }
}
