class Animal {
    name: String;
    constructor(name: String) {
        self.name <- name
    };

    speak(): String {
        "Some sound"
    };

    getName(): String {
        name
    };
};

class Dog inherits Animal {
    constructor(name: String) {
        inherit name
    };

    speak(): String {
        "Woof!"
    };

    chaseTail(): String {
        "Chasing tail!"
    };
};

class Cat inherits Animal {
    constructor(name: String) {
        inherit name
    };

    speak(): String {
        "Meow!"
    };

    climbTree(): String {
        "Climbing tree!"
    };
};

class Main {
    main(): Object {
        let dog: Dog <- new Dog("Buddy") in
        let cat: Cat <- new Cat("Whiskers") in
        let animal: Animal <- dog in
        let sound1: String <- dog.speak() in
        let sound2: String <- cat.speak() in
        let sound3: String <- animal.speak() in
        case animal of
            dog : Dog => dog.chaseTail()
            cat : Cat => cat.climbTree()
            esac;
        let animalName: String <- animal.getName() in
        let dogName: String <- dog.getName() in
        let catName: String <- cat.getName() in
        let result: String <- "Animal: " + animalName + ", Dog: " + dogName + ", Cat: " + catName + ", Sounds: " + sound1 + " " + sound2 + " " + sound3 in
        result
    };
};
