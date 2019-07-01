# twittercache 0.1.0.9000

TODO





  purrr::map_dfr(node_files, readr::read_rds)
edge_files <- data

Sys.time()

- Sanity check the export to tidygraph on my own cache / memory usage

- Wrote a utility `upgrade_cache()` to make these changes automatically if you built up a sizeable cache using 0.1.0 stuff. Timestamps inferred from the date the file `.rds` file was created.

DONE

- Store edge and node data in *uncompressed* rather than compressed `.rds` files for fast read/write, in particular faster export of the network
- Add a `sampled_at` column to node data file in the BLAH column
- Don't even add protected users to the request list
- Exported functions `get_node_data()` and `get_edge_data()`
- Added a `NEWS.md` file to track changes to the package.
- Save edges as two column tibbles of `bit64` integer64 types -- should make exporting the network fast. Also changed the `user_id` in the node data tibble to be an `integer64` type.
