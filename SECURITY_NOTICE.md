# Security Notice – FF3 Withdrawal

## Summary

FF3 and FF3-1 were withdrawn from NIST Special Publication 800-38G in 2024 following multiple cryptanalytic results. The implementations in this repository are provided solely for education, research, and interoperability testing. They must not be used to protect production data.

## Timeline

- **2017** – Durak and Vaudenay present attacks on the original FF3 design; NIST responds with FF3-1 and a reduced tweak size.
- **2021** – Beyne publishes further attacks covering both FF3 and FF3-1, demonstrating weaknesses in the tweak schedule.
- **2024** – NIST issues SP 800-38G Rev. 1 and removes FF3/FF3-1 from the standard.

## Known Issues

- Practical attacks against the tweak schedule enable plaintext recovery in bounded domains.
- Small domain usage exacerbates vulnerability to exhaustive search.
- Continued reliance on FF3 may violate regulatory guidance (PCI DSS, FIPS 140 validations, etc.).

## Intended Use

- Study the structure of format-preserving encryption.
- Compare cross-language implementations of a withdrawn standard.
- Reproduce published research results.

## Not Appropriate For

- Encrypting production data or sensitive information.
- Deploying in compliance-driven environments.
- Claiming modern cryptographic assurance.

## References

- NIST SP 800-38G Rev. 1 (2024)
- Durak, F.B. and Vaudenay, S. “Breaking the FF3 Format-Preserving Encryption Standard” (2017)
- Beyne, T. “Cryptanalysis of FF3 and FF3-1” (2021)
