Wed May 20 11:26:15 PDT 2026

Simple 'contacts' application for us in the BRICKS_TS 
transaction processing environment.

Execute with 'PERS' transaction key.

Move the *.cob files to your BRICKS_TS/runtime/cobol directory.
Move the *.map files to your BRICKS_TS/runtime/map directory.


Add these lines to BRICKS_TS/runtime/transactions.conf

PERS:cobol:pers.cob:public,users,admin
PERL:cobol:persl.cob:public,users,admin
PERV:cobol:persv.cob:public,users,admin


This was created almost entirely with Claude.ai.

Daryl Jones
daryl@tcomeng.com


