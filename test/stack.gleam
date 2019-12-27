import gleam/otp/process.{Pid}
import gleam/otp/agent.{Msg, Continue, Reply}

// Stack agent implementation

pub fn start_link(initial stack) -> Result(Pid(Msg(List(x))), String) {
  agent.start_link(fn() { agent.Ready(stack) })
}

pub fn push(onto pid: Pid(Msg(List(x))), item new: x) -> Nil {
  let push_fn = fn(stack) {
    Continue([new | stack])
  }
  agent.async(pid, push_fn)
}

pub fn pop(from pid: Pid(Msg(List(x)))) -> Result(x, Nil) {
  let pop_fn = fn(stack) {
    case stack {
      [] ->
        Reply(with: Error(Nil), then: Continue([]))

      [elem | rest] ->
        Reply(with: Ok(elem), then: Continue(rest))
    }
  }
  agent.sync(pid, pop_fn)
}

// Tests

import gleam/expect

pub fn stack_agent_test() {
  let Ok(agent) = start_link(initial: ["Hello"])

  // Popping returns the first element
  expect.equal(
    pop(from: agent),
    Ok("Hello"),
  )

  // Popping a second time returns nothing as the stack is empty
  expect.equal(
    pop(from: agent),
    Error(Nil),
  )

  push(onto: agent, item: "World!")

  expect.equal(
    pop(from: agent),
    Ok("World!"),
  )
}
