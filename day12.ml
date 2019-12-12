open Util

module ThreeD = struct
  type t = { x: int; y: int; z: int } 
  let empty = { x = 0; y = 0; z = 0; }

  let add a b =
    { x = a.x + b.x;
      y = a.y + b.y;
      z = a.z + b.z }

  let absolute_sum vec =
    abs vec.x + abs vec.y + abs vec.z

  let read () =
    read_line ()
    |> (fun line -> Scanf.sscanf line "<x=%d, y=%d, z=%d>" (fun x y z -> { x = x; y = y; z = z }))

  let gravity a b =
    let gravity_of_one x y =
      if x < y then 1
      else if x = y then 0
      else (-1) in
    { x = gravity_of_one a.x b.x;
      y = gravity_of_one a.y b.y;
      z = gravity_of_one a.z b.z  }
end

module OneD = struct
  type t = int
  let add a b = a + b
  let read () = failwith "Not Implemented: OneD#read"
  let absolute_sum x = x
  let empty = 0
  let gravity a b = 
    if a < b then 1
    else if a = b then 0
    else (-1)
end

module type MOON_POSITION = sig
  type t
  val add: (t -> t -> t)
  val read: (unit -> t)
  val absolute_sum: (t -> int)
  val empty: t
  val gravity: (t -> t -> t)
end

module Moon = struct
  module Make(MP: MOON_POSITION) = struct
    type t = {
      position: MP.t;
      velocity: MP.t;
    } 
    type moon_list = t list 
    let apply_gravity gravity moon =
      { moon with velocity = MP.add moon.velocity gravity }

    let step moon =
      { moon with position = MP.add moon.position moon.velocity }

    let potential_energy moon =
      MP.absolute_sum moon.position

    let kinetic_energy moon =
      MP.absolute_sum moon.velocity

    let energy moon =
      potential_energy moon * kinetic_energy moon

    let read () =
      { position = MP.read ();
        velocity = MP.empty }

    let gravity_of_moons a b =
      MP.gravity a.position b.position

    let empty_velocity = MP.empty
    let add_velocity a b = MP.add a b

    let step moons =
      CCList.map (fun moon ->
          let gravity = CCList.fold_left (fun gravity other_moon -> add_velocity gravity (gravity_of_moons moon other_moon))
              empty_velocity moons in
          apply_gravity gravity moon) moons
      |> CCList.map step
  end
end

module ThreeDMoon = Moon.Make(ThreeD)
module OneDMoon = Moon.Make(OneD)

let read_input () =
  read_all ThreeDMoon.read

let solve moons step_count =
  Gen.fold (fun moons _ -> ThreeDMoon.step moons) moons
    (Gen.int_range 1 step_count)
  |> CCList.map ThreeDMoon.energy
  |> CCList.fold_left (+) 0
  |> string_of_int |> print_endline

let _ =
  let moons = read_input () in
  time (fun () -> solve moons 1000)
