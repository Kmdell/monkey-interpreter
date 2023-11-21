# monkey-interpreter
Follow the book by Thorsten Ball to create an interpreter for the made up C-like scripting language Monkey Lang.

## Why two versions?
You might be wondering why there are two separate versions and why I didnt tag them or have them as separate releases, because I wanted to have them both here. So I can look back and see the evolution of my Rust understanding in just a couple of months, how I handle these challenges differently as I got better at Rust. And to show how going through this project once without any guidance helped me understand the language better and find better ways of handling the challeneges posed by converting Go to Rust.

The first version used traits and a mess of functions to help with casting and to help evaluation along, this of course is very slow. The solution? Use enums of course, which is pretty obvious now, because of Rust's strong matching system and very powerful enum system. As I went through the first time, I was still weary of enums, Rc, and Box. Coming straight from academic C and C++, the fat pointers and enums didn't make sense, I was used to handling raw pointers and the dangers that resided with them, but having the nicety of safety made sense after I started trying my hand at some of the data structures I implemented in C and C++ but in Rust. Which allowed for my uncertainty of these to get waved.

What are the benefits of v2 over v1, well all those function calls add up, and getting rid of them and just using enums and pattern matching allowed for roughly a 33% increase in speed when doing recursion (aka foing fibonacci sequence). That is all I have checked thus far, but when I have more time I will check for more. 

## What's next?
Well I want to improve this one a little more, maybe add some more features like parsing a file, or making it so that you can enter text on multiple lines, but after a well needed break. Then its the follow up book "Writing a compiler in Go" obviously. Doing this project twice back to back has been a challenge and I want to try other things for the time being, althought I do look forward to seeing a triple speed increase in the next iteration.

## How to run it?
Make sure you have the rust toolchain installed
```
git clone https://github.com/Kmdell/monkey-interpreter.git
cd monkey-interpreter
cargo run --release --bin monkey-interpreter-v2
```
You can interchange monkey-interpreter-v2 with monkey-interpreter-v1 if you want to see how the old one holds up with the new.
