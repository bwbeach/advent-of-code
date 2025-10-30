# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a command-line tool written in TypeScript. The developer is learning TypeScript, so prioritize clear, well-typed code with educational explanations when introducing new TypeScript concepts.

## Type Checking Philosophy

This project uses **strict type checking** with enhanced compiler options:
- `strict: true` - All strict type checking options enabled
- `noUncheckedIndexedAccess: true` - Array/object index access returns `T | undefined`
- `exactOptionalPropertyTypes: true` - Distinguishes between `undefined` and absent properties
- `verbatimModuleSyntax: true` - Enforces explicit type imports
- `isolatedModules: true` - Ensures each file can be compiled independently

When writing code:
- Avoid type assertions (`as`) unless absolutely necessary
- Prefer explicit types over inference for function parameters and return values
- Handle all possible undefined cases when accessing arrays or object properties
- Use discriminated unions instead of type narrowing where possible

## Development Commands

### Running TypeScript Files
```bash
# Compile and run a TypeScript file
npx tsx <file.ts>

# Or with ts-node
npx ts-node <file.ts>
```

### Type Checking
```bash
# Check types without emitting files
npx tsc --noEmit
```

### Building
```bash
# Compile TypeScript to JavaScript
npx tsc
```

## Project Structure

This is a new project currently in the setup phase. Source files will be organized as the project grows.

## TypeScript Learning Notes

Since the developer is learning TypeScript:
- Explain TypeScript-specific concepts when introducing them (generics, utility types, etc.)
- Show how strict type checking catches bugs at compile time
- Demonstrate TypeScript best practices rather than JavaScript patterns
- Provide links to TypeScript documentation for complex features
