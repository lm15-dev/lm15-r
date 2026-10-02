# MAP-17: a function tool with no description reaches every wire with no
# description key, never "description": null. The contract's
# tool_no_description cases pin the NULL wire through the vet shim; these tests
# add what canonical JSON cannot carry ("", which serializes as absent) and the
# paths no case pins (a Gemini cached prefix, a batch body, both live setup
# frames).

schema <- json_object(type = "object", properties = json_object(city = json_object(type = "string")))
weather <- function(description) function_tool("get_weather", description = description, parameters = schema)
decode <- function(wire) lm15:::.json_decode(rawToChar(wire$body))

declarations <- function(x, out = list()) {
  if (is.list(x)) {
    if (!is.null(names(x)) && identical(x[["name"]], "get_weather") &&
      any(c("parameters", "input_schema", "parametersJsonSchema") %in% names(x))) out[[length(out) + 1L]] <- x
    for (v in x) out <- declarations(v, out)
  }
  out
}
only_declaration <- function(x) {
  found <- declarations(x)
  expect_length(found, 1L)
  found[[1L]]
}

clients <- function() list(
  anthropic = new_lm("anthropic", api_key = "k", env = character()),
  openai = new_lm("openai", api_key = "k", env = character()),
  "openai-chat" = new_lm("openai-chat", api_key = "k", env = character()),
  gemini = new_lm("gemini", api_key = "k", env = character())
)

test_that("MAP-17: an absent or empty description is left off every dialect", {
  for (description in list(NULL, "")) {
    for (provider in names(clients())) {
      lm <- clients()[[provider]]
      req <- request("m-1", list(message_user("hi")), tools = list(weather(description)), config = config(max_tokens = 64L))
      decl <- only_declaration(decode(build_request(lm, req)))
      expect_false("description" %in% names(decl), label = provider)
      expect_identical(setdiff(names(decl), "type")[[1L]], "name", label = provider)
    }
  }
})

test_that("MAP-17: a present description keeps its slot after the name", {
  for (provider in names(clients())) {
    lm <- clients()[[provider]]
    req <- request("m-1", list(message_user("hi")), tools = list(weather("Weather for a city")))
    decl <- only_declaration(decode(build_request(lm, req)))
    keys <- names(decl)
    expect_identical(keys[[match("name", keys) + 1L]], "description", label = provider)
    expect_identical(decl$description, "Weather for a city")
  }
})

test_that("MAP-17: live setup frames, a Gemini cached prefix and a batch leave it off", {
  for (description in list(NULL, "")) {
    lms <- clients()
    for (pair in list(list(lms$openai, "gpt-realtime-mini"), list(lms$gemini, "gemini-3.1-flash-live-preview"))) {
      frames <- live_setup_frames(pair[[1L]], live_config(pair[[2L]], tools = list(weather(description))))
      expect_false("description" %in% names(only_declaration(frames)))
    }
    prefix <- request("gemini-2.5-flash", list(message_user("a long stable prefix")), tools = list(weather(description)))
    expect_false("description" %in% names(only_declaration(decode(cache_op_build(lms$gemini, "create", prefix = prefix, ttl_seconds = 300L)))))
    nested <- request("claude-haiku-4-5", list(message_user("hi")), tools = list(weather(description)), config = config(max_tokens = 64L))
    wire <- batch_op_build(lms$anthropic, "submit", request = batch_request(list(nested)))[[1L]]
    expect_false("description" %in% names(only_declaration(decode(wire))))
  }
})
