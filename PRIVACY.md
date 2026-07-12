# Privacy and data handling

ClinRiskR is designed to run locally. The package contains no telemetry,
network calls, cloud upload, or remote model inference.

## Defaults

- Source data are read into the local R process and are not copied to the
  output directory.
- Aggregate tables may expose small cell counts. Users must review outputs
  against their disclosure-control policy.
- Row-level IDs, outcomes, and probabilities are not written unless
  export_predictions is explicitly enabled.
- The private-data and results directories are ignored by Git.
- The public example is generated synthetically and contains no patient data.

## User responsibilities

Before using real clinical data, users must have an appropriate lawful basis,
ethics approval where required, access authorization, and a secure computing
environment. Remove direct identifiers, apply institutional minimum-cell-size
rules, and review every output before sharing it.

The package cannot determine whether a dataset is de-identified under a
particular law or policy. That decision remains with the data controller and
the responsible research institution.

## Reporting a privacy issue

Do not attach sensitive data to a public issue. Open a minimal issue without
data and ask the maintainers for a private reporting channel.
