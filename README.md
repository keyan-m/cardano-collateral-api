# Web API for Signing Transactions with Provided Collateral

This application aims to provide collateral UTxOs for users who either have not
set up theirs yet, don't have any wallets, or simply as a convenient solution
for developers to avoid dealing with their users' collateral UTxOs, or as a
fallback.

Developers can declare the UTxO given by this application as the collateral
UTxO of their transactions, provide all the required signatures, and submit
them through the endpoint provided here.

The application will review the transaction to make sure that:
  1. The transaction is valid and won't lose the provided collateral, and
  2. Is not spending the collateral UTxO.

