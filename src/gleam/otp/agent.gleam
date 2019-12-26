import gleam/atom.{Atom}
import gleam/otp/process.{Pid}

// TODO: implement sys http://erlang.org/doc/man/sys.html

pub type Next(state) {
  Next(state)
  Continue(fn() -> Next(state))
  Hibernate(state)
  Stop(reason: String)
}

pub type Start(state) {
  Ready(state: state)
  ContinueInit(fn() -> Next(state))
  Failed(reason: String)
}

pub type Reply(reply, state) {
  Reply(reply, Next(state))
}

pub type Msg(state) {
  Msg(fn(state) -> Next(state))
}

pub external fn start_link(
  fn() -> Start(state),
) -> Result(Pid(Msg(state)), String)
  = "gleam_otp_agent_native" "start_link"

// TODO: implement
pub external fn async(
  on: Pid(Msg(state)),
  exec: fn(state) -> Next(state),
) -> Nil
  = "gleam_otp_agent_native" "async"

// TODO: implement
pub external fn sync(
  on: Pid(Msg(state)),
  timeout: Int,
  exec: fn(state) -> Reply(reply, state),
) -> reply
  = "gleam_otp_agent_native" "sync"
