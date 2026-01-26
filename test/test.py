def string(n):
    return f'''
fun main{n} (m : Int): Int =
        let Left{n} x = interesting{n} 10 in
        let y = Record{n} {{
                name{n} = \"hi\";
        }} in
        x

type Either{n} \'a \'b =
        | Left{n} of \'a
        | Right{n} of \'b
end

type Record{n} 'a = {{
        name{n} : 'a;
}}


fun interesting{n} {{type \'a}} (x : Int) : Either{n} Int \'a = Left{n} x
    '''


for i in range(0,100000):
    print(string(i))
print("fun main (m : Int) : Int =")
for j in range(0,100000):
    print(f"let ret{j} = main{j} {j} in")
print("0")
