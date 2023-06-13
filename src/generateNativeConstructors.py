doc = open("bsIncludes/NativeConstructors.bs", "w")
doc.write("""/*This file is generated by generateNativeConstructors.py
 *Do not edit this file directly
 */
module "nativeConstructors" 
{
\tvoid _construct(ref bool v){v=false;}
\tvoid _copy(ref bool d, const ref bool s){d=s;}
\tvoid _move(ref bool d, ref bool s){d=s;}
\tvoid _destruct(ref bool v){}
\tvoid _construct(ref char v){v=' ';}
\tvoid _copy(ref char d, const ref char s){d=s;}
\tvoid _move(ref char d, ref char s){d=s;}
\tvoid _destruct(ref char v){}
""")
types = ["uint", "uint64", "int", "int64", "float", "double"]
for type in types:
    doc.write(f"""\tvoid _construct(ref {type} v){{v=0;}}
\tvoid _copy(ref {type} d, const ref {type} s){{d=s;}}
\tvoid _move(ref {type} d, ref {type} s){{d=s;}}
\tvoid _destruct(ref {type} v){{}}
""")

doc.write("};")