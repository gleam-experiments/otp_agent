import gleam/atom.{Atom}
import gleam/otp/stop.{Reason}
import gleam/otp/process.{Pid, UnknownMessage}

pub external type Ref;

external type NoLeak;

// TODO: Refine stop reason type so we can tell if it is an error or not

pub type Next(state) {
  Continue(state)
  Hibernate(state)
  Exec(fn() -> Next(state))
  Stop(reason: Reason)
};

pub type Start(state) {
  Ready(state: state)
  Init(fn() -> Next(state))
  Failed(reason: String)
};

pub type Reply(reply, state) {
  Reply(with: reply, then: Next(state))
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
  within: Int,
  exec: fn(state) -> Reply(reply, state),
) -> reply
  = "gleam_otp_agent_native" "sync";

pub fn sync(on agent: Pid(Msg(state)), exec fun: fn(state) -> Reply(reply, state)) -> reply {
  sync_timeout(on: agent, exec: fun, timeout: 5000)
}

external fn proc_lib_stop(
  agent: Pid(Msg(state)),
  because: Reason,
  within: Int,
) -> NoLeak
  = "proc_lib" "stop"

pub fn stop(
  agent pid: Pid(Msg(state)),
  because reason: Reason,
  within timeout: Int,
) -> Nil {
  proc_lib_stop(agent: pid, because: reason, within: timeout)
  Nil
}
