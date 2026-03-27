# AGENTS.md

This file is for coding agents working in `floskell`.

## Scope

- Repository: `floskell`, a Haskell source formatter / pretty printer.
- Build systems present: `cabal` and `stack`.
- CI uses `cabal` on macOS and Linux, across multiple GHC versions.
- No Cursor rules were found in `.cursor/rules/`.
- No `.cursorrules` file was found.
- No Copilot instructions were found at `.github/copilot-instructions.md`.

## Repository Layout

- `src/Floskell*.hs`: library modules.
- `src/main/Main.hs`: CLI executable.
- `src/main/Test.hs`: Hspec test runner.
- `src/main/Benchmark.hs`: Criterion benchmarks.
- `src/main/Markdone.hs`: markdown parsing support used by tests and benches.
- `TEST.md`: canonical markdown-based regression input.
- `styles/*.md`: expected formatter output for each predefined style.
- `floskell.cabal`: authoritative target definitions and dependencies.
- `.github/workflows/ci.yml`: authoritative CI commands.

## Preferred Workflow

- Prefer `cabal` for build, test, and verification because CI uses it.
- Use `stack` only when you specifically need the repo's resolver workflow.
- Keep changes compatible with a broad GHC range; the codebase actively supports older compilers and multiple `haskell-src-exts` versions.
- Expect CPP guards around dependency and parser-version differences.

## Build Commands

- Build everything: `cabal build all`
- Build library only: `cabal build floskell`
- Build executable only: `cabal build exe:floskell`
- Build tests only: `cabal build test:floskell-test`
- Build benchmarks only: `cabal build bench:floskell-bench`
- Configure tests and benches explicitly: `cabal configure --enable-test --enable-benchmarks --disable-documentation`
- Check install plan without building: `cabal build --dry-run`
- Open a REPL for the library: `cabal repl floskell`

## Lint / Static Checks

- There is no dedicated linter config in the repo for `hlint`, `ormolu`, `fourmolu`, or `stylish-haskell`.
- The main static baseline is a warning-clean build with `-Wall` enabled in the Cabal targets.
- Treat `cabal build all` as the most important local lint check.
- Run Cabal package validation with `cabal check`.
- If you touch public metadata or dependencies, run both `cabal check` and `cabal build all`.

## Test Commands

- Run the full test suite: `cabal test all`
- Run only the main test suite target: `cabal test floskell-test`
- Run tests with direct output: `cabal test floskell-test --test-show-details=direct`
- Run benchmarks: `cabal bench floskell-bench`
- Run the executable on a file: `cabal run floskell -- path/to/File.hs`
- Run the executable on stdin: `cabal run floskell < path/to/File.hs`

## Running A Single Test

The test suite uses Hspec from `src/main/Test.hs`, so use Hspec's match filtering.

- Run one example by exact or partial test name:
  `cabal test floskell-test --test-show-details=direct --test-options='--match "Snippet 1"'`
- Run a narrower test:
  `cabal test floskell-test --test-show-details=direct --test-options='--match "formats as expected"'`
- Run one section:
  `cabal test floskell-test --test-show-details=direct --test-options='--match "ImportDecl"'`
- Stack equivalent:
  `stack test --test-arguments='--match "Snippet 1"'`

Notes:

- Test names are generated from markdown sections plus snippet numbers.
- If you are unsure of the exact name, inspect `src/main/Test.hs` and `TEST.md` first.

## High-Value Verification Paths

- Formatter logic changed: run `cabal test floskell-test`.
- CLI behavior changed: run `cabal build exe:floskell` and at least one `cabal run floskell -- ...` smoke test.
- Config or JSON parsing changed: run `cabal test floskell-test` and exercise `--print-config`.
- Performance-sensitive pretty-printing changed: consider `cabal bench floskell-bench`.
- Packaging changed: run `cabal check`.

## Test Architecture

- Regression tests are markdown-driven.
- `TEST.md` is the canonical input corpus.
- Each file in `styles/*.md` stores the expected rendering for a predefined style.
- Many failures are easiest to diagnose by locating the referenced markdown section/snippet.

## Source Style

Follow the existing Haskell style in the repository rather than introducing a new formatter style.

- Use 4-space indentation.
- Prefer hanging indentation for long signatures, records, and import lists.
- Preserve the import grouping style: standard/library imports, then local imports, separated by blank lines.
- Keep alignment conservative and readable; do not introduce decorative alignment that the surrounding file does not use.

## Imports

- Use explicit import lists for external modules when only a few names are needed.
- Use `qualified` imports for modules like `Data.Text`, `Data.Map.Strict`, and similar namespaces.
- Use short, conventional aliases already common in the codebase: `T`, `TL`, `TB`, `TIO`, `Map`, `M`, `JSON`, `PP`.
- Open imports are acceptable for internal modules when that is already the local convention.
- Keep imports sorted/grouped consistently with the surrounding file; avoid churn-only reorderings.

## Language Pragmas And CPP

- Keep pragmas file-local and minimal.
- Common pragmas here include `CPP`, `OverloadedStrings`, `RecordWildCards`, `LambdaCase`, and targeted feature flags.
- Preserve existing CPP compatibility guards.
- When changing parser- or dependency-sensitive code, check for version branches before simplifying.
- Prefer small, localized `#if` blocks over duplicating large amounts of logic unless parser compatibility requires it.

## Types And Signatures

- Give top-level functions explicit type signatures.
- Keep helper signatures when they clarify polymorphism, laziness/strictness, or error types.
- Use `newtype` when representing a single wrapped concept.
- Prefer records for configuration and state structures.
- Prefer record updates over positional reconstruction.

## Naming

- Modules, types, and constructors use `CamelCase`.
- Functions and values use `camelCase`.
- Record fields usually carry a domain prefix, e.g. `cfg*`, `ps*`, `style*`.
- Match existing terminology: `reformat`, `pretty`, `style`, `config`, `fixities`, `imports`, `comments`.

## Error Handling

- In library-style code, prefer returning `Either String a` or `Maybe a` for expected failures.
- Keep pure formatting/parsing paths explicit about failure, as `reformat` already does.
- It is acceptable to keep `error` for test-only code and genuinely impossible internal states if the surrounding module already uses that pattern.
- IO boundary code may catch and rethrow exceptions when handling filesystem edge cases, as in `src/main/Main.hs`.

## Comments And Documentation

- Use Haddock-style module or declaration comments for exported APIs and non-obvious internals.
- Preserve carefully placed comments in tests and markdown fixtures; they are often part of regression coverage.

## Testing Conventions

- If you change formatter behavior, expect corresponding updates to style reference outputs.
- Prefer minimal changes to `TEST.md` and `styles/*.md`; broad fixture churn is hard to review.
- Test descriptions are generated, so clear section headings in markdown matter.

## Agent-Specific Guidance

- Before editing, inspect the target file's surrounding style and match it.
- Avoid repo-wide formatting passes; this project is intentionally style-sensitive.
- Keep diffs small in generated reference markdown unless the behavior change truly affects many styles.
- When in doubt, trust `floskell.cabal`, `src/main/Test.hs`, `TEST.md`, and `.github/workflows/ci.yml` over assumptions.
