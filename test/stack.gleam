import gleam/otp/process.{Pid}
import gleam/otp/agent.{Msg, Continue, Reply}
import gleam/expect

pub fn start_link(initial stack) -> Result(Pid(Msg(List(x))), String) {
  agent.start_link(fn() { agent.Ready(stack) })
}

pub fn push(onto pid: Pid(Msg(List(x))), item new: x) -> Nil {
  let push_fn = fn(stack) { Continue([new, ..stack]) }
  agent.async(pid, push_fn)
}

pub fn pop(from pid: Pid(Msg(List(x)))) -> Result(x, Nil) {
  let pop_fn = fn(stack) {
    case stack {
      [] -> Reply(with: Error(Nil), then: Continue([]))
      [elem, ..rest] -> Reply(with: Ok(elem), then: Continue(rest))
    }
  }

  agent.sync(pid, pop_fn)
}

pub fn stack_agent_test() {
  let Ok(agent) = start_link(initial: ["Hello"])

  // Popping returns the first element
  pop(from: agent)
  |> expect.equal(Ok("Hello"))

  // Popping a second time returns nothing as the stack is empty
  pop(from: agent)
  |> expect.equal(Error(Nil))

  push(onto: agent, item: "World!")

  pop(from: agent)
  |> expect.equal(Ok("World!"))
}
