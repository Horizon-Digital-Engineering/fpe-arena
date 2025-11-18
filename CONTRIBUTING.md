# Contributing to FPE-Arena

FF3 was withdrawn from NIST SP 800-38G because of published vulnerabilities. This repository preserves the algorithm for education, interoperability testing, and historical research. All contributions must respect that scope—no production-security claims, no attempts to position FF3 as a modern cipher.

---

## Before You Start
- Review `README.md`, `docs/architecture.md`, and the tool specifications in `docs/`.
- Pass all 15 NIST vectors for the language you plan to touch.
- Use the language’s preferred formatter and linter; automated checks should come back clean.
- Run `./scripts/clean.sh` before committing so the tree stays free of build artefacts.

---

## Contribution Areas
1. **Bug fixes or refactors** that keep behaviour identical to existing implementations.
2. **Documentation updates** that improve accuracy, clarity, or consistency.
3. **Tooling improvements** (build scripts, CI helpers) that apply across languages.
4. **New language ports** that follow the three-file core plus four CLI tools architecture.

All changes must preserve deterministic behaviour, pass the official vectors, and keep format-preserving semantics intact.

---

## Coding Standards
- Follow the language-specific style already present in the implementation.
- Keep modules split into `ff3_core`, `ff3_api`, and `ff3_alphabets` (or local equivalents).
- Limit external dependencies to essentials (e.g., cryptography/AES, JSON parsing).
- Command-line tools must remain flag-driven, non-interactive, and emit the agreed JSON or plain-text formats described in `docs/`.
- Use ASCII in source and documentation unless a language demands otherwise.

---

## Quality Checklist
Complete this checklist before opening a pull request:

- [ ] All 15 NIST vectors pass via the language’s validation tool.
- [ ] Unit, integration, and CLI tests succeed locally.
- [ ] `./scripts/clean.sh` leaves `git status` clean apart from intentional changes.
- [ ] Documentation updates remain concise and technically accurate.
- [ ] Security disclaimer language matches the wording already adopted in the project.

---

## Submitting Changes
1. Fork the repository and create a feature branch.
2. Make focused commits (`feat:`, `fix:`, `docs:`, etc.) with descriptive messages.
3. Include test output or reproduction steps in the pull request description.
4. Note any deviations from the standard architecture or CLI formats.
5. Be ready to discuss performance implications if your change touches core code paths.

Pull requests that fail the NIST vectors or introduce interactive prompts in the CLI tools will be declined automatically.

---

## Getting Support
- File an issue in GitHub if you find a regression or documentation gap.
- Tag which language implementation is affected and include sample inputs, keys, and tweaks.
- For general questions about FF3’s history or research, cite the relevant papers in the discussion.

Thank you for helping keep this educational archive accurate and well documented.
