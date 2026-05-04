### Parameter Relationships

- `DURATION` and `DURATION_UNIT` are a coupled pair: threshold value and measurement unit must be configured together.
- `MSCDATE` and `MSCTIME` jointly define the transaction-start time window; using only one dimension can produce incomplete temporal filtering.
- `TCODE`, `TRN_BY`, and `TRN_EX` are complementary transaction selectors; combining them helps separate direct starts from excluded/derived transaction contexts.
- `USER` and `USERID` should be aligned to the same user population to avoid conflicting inclusion logic.
- `TTEXT` depends on transaction-code resolution and language context; keep language configuration consistent for comparable outputs.
