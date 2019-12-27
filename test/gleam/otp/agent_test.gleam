import gleam/bool
import gleam/expect
import gleam/otp/agent
import gleam/otp/process
import gleam/otp/stop.{Reason, Normal}
import gleam/result

external fn sleep(Int) -> Nil = "timer" "sleep"

pub fn start_link_ready_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
}

pub fn start_link_init_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Init(fn() { agent.Continue(Nil) }) })
  expect.true(process.is_alive(pid))
}

pub fn start_link_failed_test() {
  fn() { agent.Failed("Oh no") }
  |> agent.start_link
  |> expect.equal(_, Error("Oh no"))
}

pub fn stop_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.stop(agent: pid, within: 100, because: Normal)
  expect.false(process.is_alive(pid))
}

pub fn async_next_stop_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.async(pid, fn(_) { agent.Stop(Normal) })
  sleep(10)
  expect.true(bool.negate(process.is_alive(pid)))
}

pub fn async_next_exec_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.async(pid, fn(_) { agent.Exec(fn() { agent.Continue(Nil) }) })
  sleep(10)
  expect.true(process.is_alive(pid))
}

pub fn async_next_hibernate_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.async(pid, fn(_) { agent.Hibernate(Nil) })
  sleep(10)
  expect.true(process.is_alive(pid))
}

pub fn async_next_continue_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.async(pid, fn(_) { agent.Continue(Nil) })
  sleep(10)
  expect.true(process.is_alive(pid))
}

pub fn sync_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(0) })
  let inc = fn(s) { agent.Reply(s, agent.Continue(s + 1)) }
  let call = fn() { agent.sync(on: pid, exec: inc) }

  expect.equal(call(), 0)
  expect.equal(call(), 1)
  expect.equal(call(), 2)
  expect.equal(call(), 3)
  expect.equal(call(), 4)
}
