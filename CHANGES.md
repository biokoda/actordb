**0.5.1**

- Feature: actordb:types/0, actordb:tables/1, actordb:columns/2 (useful calls when embedded)
- Feature: return number of rows changed and last insert rowid in non select statements
- Bugfix: check schema is valid before saving (did not check in all cases before), check if kv table has right name and types
- Bugfix: significantly improved actor migration reliability during cluster rebalancing
- Bugfix: for "actor type(*);..." queries in mysql console


**0.5**

- Initial public release