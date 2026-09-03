# Archived attempt: Salsa-based parser

Status: **abandoned; not an implementation baseline**

Branch snapshot: `feat/salsa-parser` at `a6cec4e9620af71d38c45f6bcc1f5b53edd9a0a2`

This document records why the Salsa-based Rust parser attempt was stopped. Commit
messages on the branch such as "finalize migration" and "workspace tests pass" describe
local implementation steps; they do not mean that the parser was accepted, merged, or
released.

## Intended outcome

The attempt was intended to replace the Rowan pipeline with a Salsa-based incremental
parser, reduce LSP and build latency, and preserve the observable behaviour of the
released compiler.

## Outcome

The attempt did not solve the project's fundamental problem: developing and maintaining
the jv parser and compiler core in Rust was not practical. It expanded the Rust parser
implementation and compatibility surface without establishing complete behavioural
parity or a sustainable implementation path.

The branch was never merged into `main` and was never released. At archival time it
diverged from `main` by 37 commits ahead and one commit behind.

## What went wrong

### 1. The new parser remained coupled to the legacy Rust pipeline

The Salsa pipeline did not replace the complete compiler core. It accepted
`jv_lexer::Token`, converted those values into its own `OwnedToken` representation,
then converted them back into legacy tokens before invoking
`jv_parser_semantics::run`.

Consequences included:

- two token representations and conversion code in the normal path;
- continued dependence on the legacy lexer and semantic pipeline;
- duplicated ownership, span, trivia, metadata, and diagnostic handling;
- additional Rust implementation complexity instead of a clean parser boundary.

This architecture did not address the original maintainability problem.

### 2. Behavioural parity was not proven

The standard-library comparison test did not require every source file to parse
successfully. Parse failures were printed with `eprintln!`, diagnostics were also only
printed, and the final assertion required merely that more than zero files succeeded.
The test could therefore pass while an unknown subset of the corpus failed or produced
errors.

There was no release-gating differential suite demonstrating equivalent jv-to-Java
output between the released compiler and the Salsa implementation.

### 3. Important parsing paths were incomplete or heuristic

The archived code contains unfinished or provisional paths, including:

- unit definitions whose bodies were consumed structurally while detailed grammar was
  left as a TODO;
- `is` and `as` recognition based on identifier lexemes rather than dedicated token
  kinds;
- compatibility and recovery paths designed to consume input and avoid parser errors
  without necessarily constructing the complete intended semantics.

Passing focused unit tests did not establish completeness for the language.

### 4. CI did not provide branch-level release evidence

`.github/workflows/salsa-ci.yml` ran only
`cargo test -p jv_parser_salsa`. Its push trigger targeted `main`, although the
implementation remained on `feat/salsa-parser`, and no pull request integrated the
branch.

It did not gate on:

- the complete workspace test suite;
- end-to-end `.jv -> .java -> .class` compatibility;
- differential output against the released compiler;
- all release platforms;
- release packaging and installation.

### 5. The primary incremental-performance target was missed

The recorded benchmark target required at least a 50% reduction for incremental
processing. The recorded result was approximately 46% and was marked `at risk`.

The benchmark report was Salsa-only, recorded its machine as "local development machine
(details not recorded)", and did not constitute a reproducible release comparison
against the authoritative compiler.

Memory measurements also showed substantial process cost when JDK modules were loaded,
including approximately 114-119 MiB at the 2,000-line case and up to approximately
306 MiB for the 40,000-line Salsa Full case. These measurements were useful diagnostic
data, but did not justify adopting the implementation.

### 6. The attempt increased implementation effort without changing feasibility

A second Rust parser experiment already existed on `feature/parser2-crate`. The Salsa
branch added another large parser, lowering, solver, compatibility, benchmark, and
diagnostic implementation. This repeated rewrite demonstrated that another Rust parser
architecture did not remove the development bottleneck.

The project owner therefore rejected the Salsa implementation and selected a Nim
compiler core behind a C ABI while preserving the Rust CLI and host integration.

## Material worth preserving

The implementation is not a migration source or current architecture. The following
artifacts may nevertheless be reviewed independently and reused when valid:

- language input fixtures and standard-library corpus lists;
- expected diagnostics and source-span cases;
- incremental, full-parse, LSP-latency, and memory benchmark scenarios;
- behavioural requirements that can be verified against released jv output;
- regression cases discovered during the experiment.

Each reused test must be strengthened so that failures cannot be logged and ignored.
Salsa-specific query structure, token ownership, compatibility adapters, and solver
design are historical implementation details and must not constrain the Nim design.

## Successor

The successor is tracked by:

- [#1 Replace the parser and compiler core with Nim behind a C ABI](https://github.com/project-jvlang/jv-lang/issues/1)
- [#3 Implement the jv lexer and parser in Nim](https://github.com/project-jvlang/jv-lang/issues/3)
- [#6 Implement lowering, IR, and Java 21/25 code generation in Nim](https://github.com/project-jvlang/jv-lang/issues/6)
- [#8 Implement semantic analysis and type inference in Nim](https://github.com/project-jvlang/jv-lang/issues/8)
- [#9 Create a Rust-Nim differential compiler test harness](https://github.com/project-jvlang/jv-lang/issues/9)

The compatibility baseline is released user-visible behaviour, not this abandoned branch.
