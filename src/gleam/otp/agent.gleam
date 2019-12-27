import gleam/atom.{Atom}
import gleam/otp/process.{Pid, UnknownMessage}

pub external type Ref;

pub type Next(state) {
  Next(state)
  Continue(fn() -> Next(state))
  Hibernate(state)
  Stop(reason: String)
};

pub type Start(state) {
  Ready(state: state)
  ContinueInit(fn() -> Next(state))
  Failed(reason: String)
};

pub type Reply(reply, state) {
  Reply(reply, Next(state))
};

pub type Msg(state) {
  Sync(Pid(UnknownMessage), Ref, fn(state) -> Next(state))
  Async(fn(state) -> Next(state))
};

pub external fn start_link(
  fn() -> Start(state),
) -> Result(Pid(Msg(state)), String)
  = "gleam_otp_agent_native" "start_link";

// TODO: test
pub fn async(on agent: Pid(Msg(state)), exec handler: fn(state) -> Next(state)) -> Nil {
  process.send(agent, Async(handler))
  Nil
}

// TODO: test
pub external fn sync(
  on: Pid(Msg(state)),
  timeout: Int,
  exec: fn(state) -> Reply(reply, state),
) -> reply
  = "gleam_otp_agent_native" "sync";
