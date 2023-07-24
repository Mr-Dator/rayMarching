#load "graphics.cma";;
open Graphics;;

let niceGreen = rgb 43 113 40;;

type vector = {x:float; y:float; z:float;};;


let add v1 v2 =
	let x1, y1, z1 = v1.x, v1.y, v1.z in
	let x2, y2, z2 = v2.x, v2.y, v2.z in
	{x=x1+.x2; y=y1+.y2; z=z1+.z2};;

let scale a v1 =
	let x1, y1, z1 = v1.x, v1.y, v1.z in
	{x=x1*.a; y=y1*.a; z=z1*.a};;

let neg v1 = scale (-1.) v1;;

let sub v1 v2 = (*return v1 - v2*)
	add v1 (neg v2);;

let length v =
	let x, y, z = v.x, v.y, v.z in
	sqrt(x *. x +. y *. y +. z *. z);;

let lengthBis v =
	let x, y, z = Float.rem v.x 1. , Float.rem v.y 1., Float.rem v.z 1. in
	sqrt(x *. x +. y *. y +. z *. z);;

let dot u v =
	let {x=a;y=b;z=c} = u in
	let {x=m; y=n; z=k} = v in
	a*.m +. b *. n +. c *. k;;

let normalize v = scale (1. /. length v) v;;

let colorVec color =
	let b = color mod 256 in
	let g = ((color - b)/256) mod 256 in
	let r = ((color-b)/(256*256)) - g/256 in
	{x= (float_of_int r); y= float_of_int g; z = float_of_int b};;

let colorOfVec v =
	let {x=r; y=g; z=b} = v in
	rgb (int_of_float r) (int_of_float g) (int_of_float b);;

let colorGrad grad = scale 127.5 (add grad {x=1.;y=1.;z=1.});;

let softMax d1 d2 =
	let k = 2.5 in
	let h = (max (k -. Float.abs(d1 -. d2)) 0.) /. k in
	min d1 d2 -. ((h ** 3.) *. k /. 6.);;

let modFloat x alpha = let y = Float.rem x alpha in
	if y >= 0. then y else (y +. alpha);;

let modVec u alpha =
	let {x=u; y=v; z=w} = u in
	{x= (modFloat u alpha); y= (modFloat v alpha); z= (modFloat w alpha)};;

let repetition p alpha f =
	let s = alpha *. 0.5 in
	let u = {x=s;y=s;z=s} in
	let v = sub (modVec (add p u) alpha) u in
	f v;;

let cap k = if k > 255. then 255. 
				else if k < 0. then 0.
				else k;;

let fog dis disMax color = cap (color -.  (255. *. (dis /. disMax)));;

let fogVec u dis disMax =
	let {x=a;y=b;z=c} = u in
	let f = fog dis disMax in
	{x= f a;y=f b; z=f c};;

let disSphere c r p = length (sub p c) -. r;;

let disSphereBis p c r = 
	let alpha = 10. in 
	let s = alpha /. 2. in
	let vec = {x=s;y=s;z=s} in
	length (sub (modVec (sub c p) alpha) vec) -. r;;

let disPlane p n h = dot p n +. h;;

let world p =
	let {x=px;y=py;z=pz} = p in
	
	let freq = 5. in
	let displacement1 = sin(freq *. px) *. sin(freq *. py) *. sin(freq *. pz) *. 0.25 in
	
	let sphere0 = disSphereBis p {x=(4.5); y=(3.5); z=7.} 1.5 in
	let sphere1 = disSphereBis p {x=(5.); y=(5.); z=(10.)} 2.0 in
	
	let plane1 = disPlane p {x=0.; y=(1.); z=0.} (10.) in
	
	min plane1 (softMax sphere1 (sphere0 +. displacement1))
	
	(*min plane1 sphere1*)
	
	(*let sphere = disSphere {x=(0.); y=(-5.); z=16.} 1.0 in
	repetition p 40. sphere*)
	;;

let normal p =

	let dx = {x=0.001; y=0.; z=0.} in
	let dy = {x=0.; y=0.001; z=0.} in
	let dz = {x=0.; y=0.; z=0.001} in
	
	let gradX = world (add p dx) -. world (sub p dx) in
	let gradY = world (add p dy) -. world (sub p dy) in
	let gradZ = world (add p dz) -. world (sub p dz) in
	
	normalize {x=gradX; y=gradY; z=gradZ};;


let rayMarch ro rd =

	let traveled = ref 0. in
	let maxStep = 500 in
	let eps = 0.0001 in
	let maxDis = 1000. in
	let i = ref 0 in
	let dis = ref ((maxDis +. eps) /. 2.) in
	let color = ref (black) in
	
	while  !i < maxStep && !dis > eps && !dis < maxDis do
		let curPos = add ro (scale !traveled rd) in
		dis := world curPos;
		if !dis < eps then
			begin
			let normalDir = normal curPos in
			let light = {x=1.; y=(3.); z=(-14.)} in
			let lightDir = normalize (sub light curPos) in
			let intensity = max 0. (dot normalDir lightDir) in
			
			(*color := colorOfVec (scale intensity (colorVec niceGreen));*)
			color := colorOfVec (fogVec (colorGrad normalDir) !traveled maxDis);
			
			end;
		traveled := !traveled +. !dis;
		i := !i + 1;
	done;
	!color;;


let t= 400;;

let draw mat =
	for i = 0 to t-1 do
		for j = 0 to t-1 do
			set_color mat.(i).(j);
			plot i j;
		done;
	done;
	;;

let render () =
	let l = 10. in
	let camPos = {x=l/.2.0; y=l/.2.; z=(0.)} in
	let ro = camPos in
	let screen = Array.make_matrix t t (rgb 0 0 0) in
	
	for i = 0 to t-1 do
		for j= 0 to t-1 do
			let a =(float_of_int i *. l /. float_of_int t  ) in
			let b = (float_of_int j *. l /. float_of_int t )in
			let pixelVec = {x=a; y=b; z=(7.)} in
			let rd = normalize (sub pixelVec camPos) in
			
			screen.(t-1-j).(t-1-i) <- rayMarch ro rd;
			(*print_float (100. *. float_of_int( (i  * t ) + (j+1)) /. float_of_int (t*t));
			print_newline (); *)
			
		done;
	done;
	make_image screen
	;;


open_graph(" " ^ string_of_int t ^"x" ^ string_of_int t);
let screen = render () in
draw_image screen 0 0;;

Float.rem (-1.) 10.;;



