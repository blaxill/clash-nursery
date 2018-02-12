*J1.Core> :vhdl
Loading dependencies took 7.33583s
Compiling: J1.Core.j1
Applied 778 transformations
Normalisation took 2.917522s
Netlist generation took 0.040449s
Compiling: J1.Core.j1WithRam
J1.Core.$sreadNew21391119 (:: GHC.Classes.IP
  rst
  (Clash.Signal.Internal.Reset
     (Clash.Signal.Internal.Dom system 10000)
     Clash.Signal.Internal.Asynchronous)
-> GHC.Classes.IP
     clk
     (Clash.Signal.Internal.Clock
        (Clash.Signal.Internal.Dom system 10000)
        Clash.Signal.Internal.Source)
-> Clash.Signal.Internal.Clock
     (Clash.Signal.Internal.Dom system 10000)
     Clash.Signal.Internal.Source
-> Clash.Signal.Internal.Signal
     (Clash.Signal.Internal.Dom system 10000)
     (Clash.Sized.Internal.Unsigned.Unsigned
        (GHC.TypeLits.Extra.CLog 2 32))
-> Clash.Signal.Internal.Signal
     (Clash.Signal.Internal.Dom system 10000)
     (GHC.Base.Maybe
        (GHC.Tuple.(,)
           (Clash.Sized.Internal.Unsigned.Unsigned
              (GHC.TypeLits.Extra.CLog 2 32))
           (Clash.Sized.Internal.BitVector.BitVector 16)))
-> Clash.Sized.Internal.BitVector.BitVector 16)
has potentially dangerous meta-stability issues:

The following clocks:
* GHC.Classes.IP
  clk
  (Clash.Signal.Internal.Clock
     (Clash.Signal.Internal.Dom system 10000)
     Clash.Signal.Internal.Source)
* Clash.Signal.Internal.Clock
  (Clash.Signal.Internal.Dom system 10000)
  Clash.Signal.Internal.Source
belong to the same clock domain and should be connected to the same clock source in order to prevent meta-stability issues.
Applied 1511 transformations
Normalisation took 4.027786s
Netlist generation took 0.051868s
Total compilation took 14.42948s

