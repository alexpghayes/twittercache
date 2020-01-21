sample_snowball <- function(
  users, hops,
  direction = c(
    "following",
    "followed-by",
    "both"
  )) {

  if (hops < 0)
    stop("`hops` must be greater than zero.")

  direction <- rlang::arg_match(direction)

  for (user in users)
    if (!users_in_cache(user))
      add_users_to_cache(user)

  # TODO: respect direction in the future

  friends <- cache_get_friends(users)

  sample_snowball(friends, hops - 1)
}
