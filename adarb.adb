with Ada.Numerics.Elementary_Functions;use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;
with Interfaces;use Interfaces;
with Ada.Text_IO;

procedure Adarb is
   
   MAX_DEPTH : constant := 5; 
   WIDTH     : constant := 1280;
   HEIGTH    : constant := 720;
   SAMPLES   : constant := 50;
   
   ---------------------------------------------------------------------------
   
   type v3 is record
      X, Y, Z : Float;
   end record;
   
   type v3_matrix is array(Positive range <>, Positive range <>) of aliased v3;
   type v3_matrix_access is access v3_matrix;
   
   procedure Free_v3_matrix is new 
     Ada.Unchecked_Deallocation(Object => v3_matrix,
                                Name   => v3_matrix_access);
   ---------------------------------------------------------------------------
   
   function v3_new(X, Y , Z : Float) return v3 is (v3'(X, Y, Z));
   
   ---------------------------------------------------------------------------
   
   procedure v3_add(dest : out v3;
                    a, b : v3)
   is
   begin
      dest.X := a.X + b.X;
      dest.Y := a.Y + b.Y;
      dest.Z := a.Z + b.Z;
   end v3_add;
   
   ---------------------------------------------------------------------------
   
   procedure v3_mul(dest : out v3;
                    a, b : v3)
   is
   begin
      dest.X := a.X * b.X;
      dest.Y := a.Y * b.Y;
      dest.Z := a.Z * b.Z;
   end v3_mul;
   
   ---------------------------------------------------------------------------
   
   procedure v3_sub(dest : out v3;
                    a, b : v3)
   is
   begin
      dest.X := a.X - b.X;
      dest.Y := a.Y - b.Y;
      dest.Z := a.Z - b.Z;
   end v3_sub;
   
   ---------------------------------------------------------------------------
   
   function v3_dot(a, b : v3) return Float 
   is (a.X * b.X + a.Y * b.Y + a.Z * b.Z);
   
   ---------------------------------------------------------------------------
   
   procedure v3_muls(dest : out v3;
                     a    : v3;
                     u    : Float)
   is
   begin
      dest.X := a.X * u;
      dest.Y := a.Y * u;
      dest.Z := a.Z * u;
   end v3_muls;
   
   ---------------------------------------------------------------------------
   
   procedure v3_divs(dest : out v3;
                     a    : v3;
                     u    : Float)
   is
   begin
      dest.X := a.X / u;
      dest.Y := a.Y / u;
      dest.Z := a.Z / u;
   end v3_divs;   
   
   ---------------------------------------------------------------------------
   
   function v3_norm(v : v3) return Float is 
     (Sqrt(v.X * v.X + v.Y * v.Y + v.Z * v.Z));
   
   ---------------------------------------------------------------------------
   
   procedure v3_mkunit(dest : out v3;
                       v    : v3)
   is
      n : constant Float := v3_norm(v);
   begin
      dest.X := v.X / n;
      dest.Y := v.Y / n;
      dest.Z := v.Z / n;
   end v3_mkunit;
   
   ---------------------------------------------------------------------------
   
   type Ray is record
      Origin, Direction : v3;
   end record;
   
   ---------------------------------------------------------------------------
   
   procedure Ray_Point(dest : out v3;
                       r    : Ray;
                       t    : Float)
   is
   begin
      dest.X := r.Origin.X + r.Direction.X * t;
      dest.Y := r.Origin.Y + r.Direction.Y * t;
      dest.Z := r.Origin.Z + r.Direction.Z * t;
   end Ray_Point;
   
   ---------------------------------------------------------------------------
   
   type Camera is record
      Eye, Lt, Rt, Lb : v3;
   end record;
   
   ---------------------------------------------------------------------------
   
   type Sphere is record
      Center : v3;
      Radius : Float;
      Color  : v3;
      Is_Light : Boolean;
   end record;

   type Sphere_Array is array(Positive range <>) of aliased Sphere;
   
   ---------------------------------------------------------------------------
   
   function Sphere_New(Center : v3;
                       Radius : Float;
                       Color  : v3) return Sphere
   is (Sphere'(Center => Center, 
               Radius => Radius, 
               Color    => Color, 
               Is_Light => False));
   
   ---------------------------------------------------------------------------
   
   type World is record
      Sphere_Count : Natural;
      Spheres      : Sphere_Array(1 .. 8);
      Cam          : Camera;
   end record;
   
   ---------------------------------------------------------------------------
   
   function World_New return World 
   is
      Res : World;
   begin
      Res.Sphere_Count := 8;
      
      Res.Spheres(1) := Sphere_New(v3_new(0.0, -10002.0, 0.0), 
                                   9999.0, 
                                   v3_new(1.0, 1.0, 1.0));
      Res.Spheres(2) := Sphere_New(v3_new(-10012.0, 0.0, 0.0), 
                                   9999.0, 
                                   v3_new(1.0, 0.0, 0.0));
      Res.Spheres(3) := Sphere_New(v3_new(10012.0, 0.0, 0.0), 
                                   9999.0, 
                                   v3_new(0.0, 1.0, 0.0));
      Res.Spheres(4) := Sphere_New(v3_new(0.0, 0.0, -10012.0), 
                                   9999.0, 
                                   v3_new(1.0, 1.0, 1.0));
      Res.Spheres(5) := Sphere_New(v3_new(0.0, 10012.0, 0.0), 
                                   9999.0, 
                                   v3_new(1.0, 1.0, 1.0));
      Res.Spheres(5).Is_Light := True;
     
      Res.Spheres(6) := Sphere_New(v3_new(-5.0, 0.0, 2.0), 
                                   2.0, 
                                   v3_new(1.0, 1.0, 0.0));
      Res.Spheres(7) := Sphere_New(v3_new(0.0, 5.0, -1.0), 
                                   4.0, 
                                   v3_new(1.0, 0.0, 0.0));
      Res.Spheres(8) := Sphere_New(v3_new(8.0, 5.0, -1.0), 
                                   2.0, 
                                   v3_new(0.0, 0.0, 1.0));
      
      Res.Cam.Eye.X := 0.0;
      Res.Cam.Eye.Y := 4.5;
      Res.Cam.Eye.Z := 75.0;
      
      Res.Cam.Lt.X := -8.0;
      Res.Cam.Lt.Y := 9.0;
      Res.Cam.Lt.Z := 50.0;
      
      Res.Cam.Rt.X := 8.0;
      Res.Cam.Rt.Y := 9.0;
      Res.Cam.Rt.Z := 50.0;
      
      Res.Cam.Lb.X := -8.0;
      Res.Cam.Lb.Y := 0.0;
      Res.Cam.Lb.Z := 50.0;
      
      return Res;
   end World_New;
      
   ---------------------------------------------------------------------------
   
   type Hit is record
      Dist : Float;
      Point, Normal : v3;
   end record;
   
   ---------------------------------------------------------------------------
   
   function Hit_Sphere(sp : Sphere; 
                       r : Ray;
                       h : in out Hit) return Boolean
   is
      oc : v3;
      a, b, c, dis : Float;
   begin
      v3_sub(oc, r.Origin, sp.Center);
      
      a := v3_dot(r.Direction, r.Direction);
      b := v3_dot(oc, r.Direction);
      c := v3_dot(oc, oc) - (sp.Radius * sp.Radius);
      dis := b*b - a*c;
      
      if(dis > 0.0) then
         declare
            e : Float := Sqrt(dis);
            t : Float := (-b-e) / a;
         begin
            if(t > 0.007) then
               h.Dist := t;
               Ray_Point(h.Point, r, t);
               v3_sub(h.Normal, h.Point, sp.Center);
               v3_mkunit(h.Normal, h.Normal);
               return True;
            end if;
            
            t := (-b + e) / a;
            
            if(t > 0.007) then
               h.Dist := t;
               Ray_Point(h.Point, r, t);
               v3_sub(h.Normal, h.Point, sp.Center);
               v3_mkunit(h.Normal, h.Normal);
               return True;
            end if;
            
            return False;
         end;
      end if;
      return False;
   end Hit_Sphere;
   
   ---------------------------------------------------------------------------
   
   x : Unsigned_32 := 123456789;
   y : Unsigned_32 := 362436069;
   z : Unsigned_32 := 521288629;
   w : Unsigned_32 := 88675123;
   
   function xor128 return Unsigned_32
   is
      t : Unsigned_32;
   begin
      t := x xor Interfaces.Shift_Left(x, 11);
      x := y;
      y := z;
      z := w;
      w := w xor Interfaces.Shift_Right(w, 19) xor 
        (t xor Interfaces.Shift_Right(t, 8));
      return w;
   end xor128;
   
   ---------------------------------------------------------------------------
   
   function randf return Float 
   is (Float(xor128)/Float(Unsigned_32'Last));
      
   ---------------------------------------------------------------------------
   
   function rnd_dome(normal : v3)return v3
   is
      p : v3;
      d : Float;
   begin
      loop
         p.X := 2.0 * randf - 1.0;
         p.Y := 2.0 * randf - 1.0;
         p.Z := 2.0 * randf - 1.0;
         
         v3_mkunit(p, p);
         
         d := v3_dot(p, normal);
         exit when d > 0.0;
      end loop;
      return p;
   end rnd_dome;
   
   ---------------------------------------------------------------------------
   
   function trace(w     : in out World;
                  r     : in out Ray;
                  depth : Unsigned_16) return v3
   is
      Color   : v3 := v3'(0.0, 0.0, 0.0);
      Did_Hit : Boolean := False;
      H       : Hit;
      Sp      : Positive;
   begin
      H.Dist := Float(10#1#e15);
      
      for i in w.Spheres'Range loop
         declare
            res : Hit;
         begin
            if(Hit_Sphere(w.Spheres(i), r, res)) then
               if(res.Dist > 0.0001 and then res.Dist < h.Dist) then
                  Sp := i;
                  Did_Hit := True;
                  Color := w.Spheres(i).Color;
                  H := res;
               end if;
            end if;
         end;
      end loop;
      
      if(Did_Hit and then depth < MAX_DEPTH) then
         if(not w.Spheres(Sp).Is_Light)then
            declare
               nray : Ray;
               ncolor : v3 := v3'(0.0, 0.0, 0.0);
               atColor : Float;
            begin
               nray.Origin := H.Point;
               nray.Direction := rnd_dome(h.Normal);
               ncolor := trace(w, nray, depth + 1);
               atColor := v3_dot(nray.Direction, h.Normal);
               v3_muls(ncolor, ncolor, atColor);
               v3_mul(Color, Color, ncolor);
            end;
         else
            Color := w.Spheres(Sp).Color;
         end if;
      end if;
      
      if(not Did_Hit or else depth >= MAX_DEPTH) then
         Color := v3'(0.0, 0.0, 0.0);
      end if;
      
      return Color;
   end trace;

   ---------------------------------------------------------------------------
   
   procedure writeppm(data : v3_matrix)
   is
      use Ada.Text_IO;
      F : File_Type;
   begin
      Create(F, Out_File, "adarb.ppm");
      Put_Line(F, "P3");
      Put_Line(F, WIDTH'Img & HEIGTH'Img);
      Put_Line(F, "255");
      
      for y in data'Range(2) loop
         for x in data'Range(1) loop
            declare
               Xout : Unsigned_16 := Unsigned_16(data(x, y).X * 255.99);
               Yout : Unsigned_16 := Unsigned_16(data(x, y).Y * 255.99);
               Zout : Unsigned_16 := Unsigned_16(data(x, y).Z * 255.99);
            begin
               Put(F, Xout'Img & Yout'Img & Zout'Img & " ");
            end;
         end loop;
         New_Line(F);
      end loop;
      Close(F);
   end writeppm;
   
   ---------------------------------------------------------------------------
   
   wd   : World            := World_New;
   data : v3_matrix_access := new v3_matrix(1 .. WIDTH, 1 .. HEIGTH);
   vdu  : v3               := v3'(0.0, 0.0, 0.0);
   vdv  : v3               := v3'(0.0, 0.0, 0.0);
begin
   v3_sub(vdu, wd.Cam.Rt, wd.Cam.Lt);
   v3_divs(vdu, vdu, Float(WIDTH));
   
   v3_sub(vdv, wd.Cam.Lb, wd.Cam.Lt);
   v3_divs(vdv, vdv, Float(HEIGTH));   
      
   for y in data'Range(2) loop
      for x in data'Range(1) loop
         declare
            r : Ray;
            u, v, c : v3;
         begin
            r.Origin := wd.Cam.Eye;
            c := v3'(0.0, 0.0, 0.0);
            
            for i in 1 .. SAMPLES loop
               r.Direction := wd.Cam.Lt;
               v3_muls(u, vdu, Float(x) + randf);
               v3_muls(v, vdv, Float(y) + randf);
               
               v3_add(r.Direction, r.Direction, u);
               v3_add(r.Direction, r.Direction, v);
               
               v3_sub(r.Direction, r.Direction, r.Origin);
               
               v3_mkunit(r.Direction, r.Direction);
               u := trace(wd, r, 0);
               v3_add(c, c, u);
            end loop;
            
            v3_divs(c, c, Float(SAMPLES));
            data.all(x, y) := c;
         end;
      end loop;
   end loop;
   
   writeppm(data.all);
   
   Free_v3_matrix(data);
end Adarb;
