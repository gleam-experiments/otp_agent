import gleam/otp/agent
import gleam/otp/process
import gleam/expect
import gleam/result
import gleam/bool

external fn sleep(Int) -> Nil = "timer" "sleep"

pub fn start_link_ready_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
}

pub fn start_link_continue_init_test() {
  let Ok(pid) = agent.start_link(fn() { agent.ContinueInit(fn() { agent.Next(Nil) }) })
  expect.true(process.is_alive(pid))
}

pub fn start_link_failed_test() {
  fn() { agent.Failed("Oh no") }
  |> agent.start_link
  |> expect.equal(_, Error("Oh no"))
}

pub fn async_next_stop_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.async(pid, fn(_) { agent.Stop("nope") })
  sleep(10)
  expect.true(bool.negate(process.is_alive(pid)))
}

pub fn async_next_continue_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.async(pid, fn(_) { agent.Continue(fn() { agent.Next(Nil) }) })
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

pub fn async_next_next_test() {
  let Ok(pid) = agent.start_link(fn() { agent.Ready(Nil) })
  expect.true(process.is_alive(pid))
  agent.async(pid, fn(_) { agent.Next(Nil) })
  sleep(10)
  expect.true(process.is_alive(pid))
}
