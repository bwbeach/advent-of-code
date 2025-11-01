#!/usr/bin/env node

// Simple CLI tool demonstrating TypeScript with Node.js types
import process from "node:process";

function greet(name: string | undefined): string {
  // TypeScript's noUncheckedIndexedAccess helps us handle undefined safely
  if (name === undefined) {
    return "Hello, World!";
  }
  return `Hello, ${name}!`;
}

// Get the name from command line arguments
// process.argv[0] is node, process.argv[1] is the script path
const name: string | undefined = process.argv[2];

const message: string = greet(name);
console.log(message);

// Return exit code 0 for success
process.exit(0);
