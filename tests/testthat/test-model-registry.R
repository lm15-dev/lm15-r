test_that("registries resolve exact names, aliases and ambiguity", {
  a <- model_info("alpha", "openai", "openai_responses", aliases = list("alias"))
  registry <- new_model_registry(list(a))
  expect_identical(registry$get("openai", "alias"), a)
  expect_identical(registry$resolve("alias"), a)
  expect_error(registry$add(a, replace = FALSE), "already registered")
  registry$add(model_info("alpha", "anthropic", "anthropic_messages"))
  expect_null(registry$resolve("alpha"))
  expect_length(registry$list("openai"), 1L)
  expect_identical(registry$providers(), c("anthropic", "openai"))
  registry$add(model_info("alpha", "openai", "openai_responses"))
  expect_null(registry$get("openai", "alias"))
})

test_that("catalog loaders are sorted, atomic per catalog, and first-wins", {
  a <- model_info("alpha", "openai", "openai_responses")
  b <- model_info("alpha", "openai", "openai_responses", aliases = list("later"))
  expect_warning(registry <- discover_model_registry(catalogs = list(
    z = function() list(as_dict(b)),
    broken = function() list(as_dict(a), json_object(id = "bad")),
    a = function() list(as_dict(a)))), "could not be read")
  expect_identical(registry$list(), list(a))
  expect_null(registry$get("openai", "later"))
})

test_that("installed catalogs are data only, and registry changes reach routers", {
  library <- tempfile(); dir.create(file.path(library, "catalog", "lm15"), recursive = TRUE)
  on.exit(unlink(library, recursive = TRUE))
  a <- model_info("bespoke", "openai", "openai_responses")
  writeLines(as_json(json_array(as_dict(a))), file.path(library, "catalog", "lm15", "model-catalog.json"))
  registry <- discover_model_registry(libraries = library)
  router <- new_router(catalog = registry, env = character())
  expect_identical(resolve(router, "bespoke")$source, "catalog")
  registry$add(model_info("second", "gemini", "gemini_generate_content"))
  expect_identical(resolve(router, "second")$provider, "gemini")
})

test_that("estimate_cost is exact or NULL (spec/types.md, InferencePricing)", {
  prices <- inference_pricing(input_per_million = 2, output_per_million = 5)
  known <- function(i, o) usage(input_tokens = i, output_tokens = o, cache_read_tokens = 0L, cache_write_tokens = 0L)
  expect_identical(estimate_cost(prices, known(1000000L, 2000000L)), 12)
  # Unknown counts make the cost unknown, never a smaller number.
  expect_null(estimate_cost(prices, usage(input_tokens = 1000000)))
  expect_null(estimate_cost(prices, usage()))
  expect_null(estimate_cost(prices, usage(input_tokens = 1000000L, output_tokens = 2000000L)))
  # An unknown cache split is harmless when cached tokens cost the same as input.
  flat <- inference_pricing(input_per_million = 2, output_per_million = 5, cache_read_per_million = 2, cache_write_per_million = 2)
  expect_identical(estimate_cost(flat, usage(input_tokens = 1000000L, output_tokens = 2000000L)), 12)
  sol <- inference_pricing(input_per_million = 2, output_per_million = 10, cache_read_per_million = 0.1, cache_write_per_million = 2.5,
    tiers = list(pricing_tier(272000L, input_per_million = 4, output_per_million = 15, cache_read_per_million = 0.2, cache_write_per_million = 5)))
  expect_identical(estimate_cost(sol, usage(input_tokens = 100000L, output_tokens = 1000L, cache_read_tokens = 90000L, cache_write_tokens = 0L)), 0.039)
  expect_identical(estimate_cost(sol, usage(input_tokens = 300000L, output_tokens = 1000L, cache_read_tokens = 290000L, cache_write_tokens = 0L)), 0.113)
  expect_error(inference_pricing(tiers = list(pricing_tier(2L), pricing_tier(1L))), "ascending")
})
