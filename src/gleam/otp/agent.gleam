import gleam/atom.{Atom}
import gleam/otp/process.{Pid, UnknownMessage}

pub external type Ref;

// TODO: Refine stop reason type so we can tell if it is an error or not

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

pub fn async(on agent: Pid(Msg(state)), exec handler: fn(state) -> Next(state)) -> Nil {
  process.send(agent, Async(handler))
  Nil
}

pub external fn sync_timeout(
  on: Pid(Msg(state)),
  timeout: Int,
  exec: fn(state) -> Reply(reply, state),
) -> reply
  = "gleam_otp_agent_native" "sync";

pub fn sync(on agent: Pid(Msg(state)), exec fun: fn(state) -> Reply(reply, state)) -> reply {
  sync_timeout(on: agent, exec: fun, timeout: 5000)
}
