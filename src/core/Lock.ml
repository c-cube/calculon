
type 'a t = {
  m: Mutex.t;
  v: 'a;
}

let create v : _ = { v; m=Mutex.create() }

let with_ (self:_ t) f =
  Mutex.lock self.m;
  Fun.protect ~finally:(fun () -> Mutex.unlock self.m)
    (fun () -> f self.v)
