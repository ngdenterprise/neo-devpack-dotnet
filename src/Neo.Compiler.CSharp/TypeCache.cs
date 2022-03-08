// Copyright (C) 2015-2021 The Neo Project.
// 
// The Neo.Compiler.CSharp is free software distributed under the MIT 
// software license, see the accompanying file LICENSE in the main directory 
// of the project or http://www.opensource.org/licenses/mit-license.php 
// for more details.
// 
// Redistribution and use in source and binary forms with or without
// modifications are permitted.

extern alias scfx;

using Microsoft.CodeAnalysis;
using System;
using System.Collections.Generic;

namespace Neo.Compiler
{
    class TypeCache
    {
        readonly Compilation compilation;
        readonly Dictionary<string, INamedTypeSymbol> cache = new();

        public TypeCache(Compilation compilation)
        {
            this.compilation = compilation;
        }

        public INamedTypeSymbol FindType(string name)
        {
            if (!cache.TryGetValue(name, out var symbol))
            {
                symbol = compilation.GetTypeByMetadataName(name) ?? throw new Exception($"{name} type not found");
                cache.Add(name, symbol);
            }
            return symbol;
        }

        public bool Equals(INamedTypeSymbol symbol, string typeName) 
            => SymbolEqualityComparer.Default.Equals(symbol, FindType(typeName));

    }
}
