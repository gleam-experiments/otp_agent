import gleam/otp/agent
import gleam/expect
import gleam/result

pub fn start_link_test() {
  agent.start_link(fn() { agent.Ready(Nil) })
  |> result.is_ok
  |> expect.true

  agent.start_link(fn() { agent.ContinueInit(fn() { agent.Next(Nil) }) })
  |> result.is_ok
  |> expect.true

  fn() { agent.Failed("Oh no") }
  |> agent.start_link
  |> expect.equal(_, Error("Oh no"))
}
