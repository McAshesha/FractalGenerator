# 🌠 FractalGenerator: Haskell CLI Fractal Explorer  

*A terminal-based fractal generator that combines mathematical beauty with ASCII art—built in Haskell with type-safe elegance!*  

## ✨ Features  
- **🎮 Interactive terminal UI**  
  ASCII-art welcome screen, guided menus, and seamless navigation.  
- **🌌 Multiple fractals**  
  Render **Mandelbrot Set**, **Julia Set**, and **Sierpinski Triangle** with configurable parameters.  
- **🖥️ Dynamic resizing**  
  Adapts to your terminal dimensions for crisp rendering (thanks to `terminal-size`).  
- **🌈 ANSI colors**  
  Vibrant fractal displays powered by `ansi-terminal`.  
- **🧪 Test-driven design**  
  Built with Haskell best practices: type safety, parallelism, and `hspec` unit tests.  

## 🏗️ Project Structure  
```bash  
app/
└── Main.hs
test/
└── Spec.hs
src/
├── Types.hs
├── CLI/
│   └── UI.hs
├── Fractals/
│   ├── Generator/
│   │   ├── Mandelbrot.hs
│   │   ├── Julia.hs
│   │   └── Sierpinski.hs
│   └── Generator.hs
├── AsciiRenderer.hs
└── Utils.hs
```  

## 🚀 Quick Start
1. **Install Stack**:
   ```bash  
   curl -sSL https://get.haskellstack.org/ | sh  
   ```  
2. **Run the app**:
   ```bash  
   stack run  # Follow on-screen menus to explore fractals!
   ```  

## 🧩 Modules Breakdown
| Module                  | Purpose                                  |  
|-------------------------|------------------------------------------|  
| `CLI.UI`                | Handles menus, prompts, and ASCII art UI |  
| `Fractals.Generator.*`  | Math logic for each fractal type         |  
| `AsciiRenderer`         | Converts fractal data to colored ASCII   |  
| `Utils`                 | Terminal size detection                  |  
| `Types`                 | Shared data types (e.g., `TerminalSize`) |  

## 📚 Key Dependencies
- [`ansi-terminal`](https://hackage.haskell.org/package/ansi-terminal): ANSI color support
- [`terminal-size`](https://hackage.haskell.org/package/terminal-size): Dynamic terminal resizing
- [`hspec`](https://hackage.haskell.org/package/hspec): Unit testing framework

## 🔧 Extending the Project
**Adding a new fractal**:
1. Create a module under `Fractals/Generator/` (e.g., `KochSnowflake.hs`).
2. Implement the `generate` function with your fractal logic.
3. Extend the `FractalChoice` type in `Generator.hs`.
4. Update `displayFractalMenu` in `CLI.UI` to include your new option.

**Tweaking parameters**:
- Modify iteration limits in `Mandelbrot.hs`/`Julia.hs`.
- Adjust color mappings in `AsciiRenderer.hs`.

## 📜 License
MIT © 2023  —  Feel free to fork, modify, and create your own fractal universe!
