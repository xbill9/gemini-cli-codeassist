import random
import time

class Mascot:
    def __init__(self, name, language, hp, special_move, catchphrase):
        self.name = name
        self.language = language
        self.hp = hp
        self.max_hp = hp
        self.special_move = special_move
        self.catchphrase = catchphrase

    def is_alive(self):
        return self.hp > 0

    def attack(self, opponent):
        damage = random.randint(10, 25)
        is_crit = random.random() < 0.2
        
        print(f"\n{self.name} attacks {opponent.name}!")
        
        if is_crit:
            damage += 15
            print(f"*** CRITICAL HIT! {self.name} uses {self.special_move}! ***")
            if self.name == "Ferris":
                print(f"Ferris shouts: 'It's a borrowing violation!'")
            elif self.name == "The Gopher":
                print(f"The Gopher launches 10,000 tiny goroutines!")
            elif self.name == "Duke":
                print(f"Duke throws a FactoryFactoryStrategyPatternException!")
            elif self.name == "Python":
                print(f"Python simply states: 'import victory'")
        
        opponent.take_damage(damage)
        print(f"{self.name}: \"{self.catchphrase}\"\n")

    def take_damage(self, amount):
        self.hp -= amount
        if self.hp < 0:
            self.hp = 0
        print(f"{self.name} takes {amount} damage! (HP: {self.hp}/{self.max_hp})")

def battle_royale():
    mascots = [
        Mascot("Ferris the Crab", "Rust", 100, "Borrow Checker Guillotine", "Memory safety is non-negotiable!"),
        Mascot("The Gopher", "Go", 100, "Panic & Recover", "I'll be done before you start."),
        Mascot("ElePHPant", "PHP", 110, "$_GET['REKT']", "It's not a bug, it's a feature."),
        Mascot("Duke", "Java", 120, "Verbose Exception Stacktrace", "Write once, run anywhere... eventually."),
        Mascot("Python Snake", "Python", 90, "Dynamic Typing Strike", "Readability counts, but so does winning."),
        Mascot("Octocat", "GitHub", 95, "Merge Conflict", "Force push initiated!")
    ]

    print("--- WELCOME TO THE PROGRAMMING MASCOT BATTLE ROYALE! ---")
    print("In the red corner... we have memory safety fanatics!")
    print("In the blue corner... we have dynamic typing enthusiasts!")
    print("FIGHT!\n")
    time.sleep(1)

    round_num = 1
    while len(mascots) > 1:
        # Pick two random fighters
        fighter1 = random.choice(mascots)
        fighter2 = random.choice(mascots)
        while fighter1 == fighter2:
            fighter2 = random.choice(mascots)

        print(f"--- ROUND {round_num}: {fighter1.name} vs {fighter2.name} ---")
        
        # Fight logic
        fighter1.attack(fighter2)
        if fighter2.is_alive():
            fighter2.attack(fighter1)
        else:
            print(f"\n-> {fighter2.name} has been GARBAGE COLLECTED!")
            mascots.remove(fighter2)

        if not fighter1.is_alive():
            print(f"\n-> {fighter1.name} has been GARBAGE COLLECTED!")
            mascots.remove(fighter1)
            
        round_num += 1
        time.sleep(0.5)

    print(f"\n\n🏆 THE WINNER IS: {mascots[0].name} ({mascots[0].language})! 🏆")
    print(f"Final HP: {mascots[0].hp}")

if __name__ == "__main__":
    battle_royale()
