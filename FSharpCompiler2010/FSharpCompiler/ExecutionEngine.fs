namespace FSharpCompiler

open System
open System.Runtime.InteropServices

module EEInternals =
  type AllocationType =
    | COMMIT=0x1000u

  type MemoryProtection =
    | EXECUTE_READWRITE=0x40u

  type FreeType =
    | DECOMMIT = 0x4000u

  [<DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true)>]
  extern IntPtr LoadLibrary(string fileName);

  [<DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true)>]
  extern IntPtr GetModuleHandle(string fileName);

  [<DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true)>]
  extern uint32 GetProcAddress(IntPtr hModule, string fn);

  let kernel32 = LoadLibrary @"kernel32.dll"
  let heapAlloc = GetProcAddress(kernel32, "HeapAlloc") // heap, flags, size
  let heapCreate = GetProcAddress(kernel32, "HeapCreate") // flags, size, maxSize

  [<DllImport("kernel32.dll", SetLastError=true)>]
  extern IntPtr VirtualAlloc(IntPtr lpAddress, UIntPtr dwSize, AllocationType flAllocationType, MemoryProtection flProtect);

  [<DllImport("kernel32.dll", SetLastError=true)>]
  extern bool VirtualFree(IntPtr lpAddress, UIntPtr dwSize, FreeType freeType);

  [<UnmanagedFunctionPointer(CallingConvention.Cdecl)>] 
  type Ret1ArgDelegate = delegate of int32 -> int32

open EEInternals

type ExecutionEngine(code: byte []) =
  let code = [|139uy; 68uy; 36uy; 4uy; 80uy; 139uy; 68uy; 36uy; 0uy; 131uy; 196uy; 4uy; 195uy|]
  let executableMemory = VirtualAlloc(IntPtr.Zero, UIntPtr(uint32(code.Length)), AllocationType.COMMIT, MemoryProtection.EXECUTE_READWRITE)
  let jitedFun = Marshal.GetDelegateForFunctionPointer(executableMemory, typeof<Ret1ArgDelegate>) :?> Ret1ArgDelegate

  do
    Marshal.Copy(code, 0, executableMemory, code.Length)

  member __.Invoke n = jitedFun.Invoke n

  interface System.IDisposable with
    member __.Dispose() =
      VirtualFree(executableMemory, UIntPtr.Zero, FreeType.DECOMMIT) |> ignore

(*
open System
open System.Runtime.InteropServices

module ExecutionEngineInternal =
  type AllocationType =
    | COMMIT=0x1000u

  type MemoryProtection =
    | EXECUTE_READWRITE=0x40u

  type FreeType =
    | DECOMMIT = 0x4000u

  [<DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true)>]
  extern IntPtr LoadLibrary(string fileName);

  [<DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true)>]
  extern IntPtr GetModuleHandle(string fileName);

  [<DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true)>]
  extern uint32 GetProcAddress(IntPtr hModule, string fn);

  let kernel32 = LoadLibrary @"kernel32.dll"
  let heapAlloc = GetProcAddress(kernel32, "HeapAlloc") // heap, flags, size
  let heapCreate = GetProcAddress(kernel32, "HeapCreate") // flags, size, maxSize

  [<DllImport("kernel32.dll", SetLastError=true)>]
  extern IntPtr VirtualAlloc(IntPtr lpAddress, UIntPtr dwSize, AllocationType flAllocationType, MemoryProtection flProtect);

  [<DllImport("kernel32.dll", SetLastError=true)>]
  extern bool VirtualFree(IntPtr lpAddress, UIntPtr dwSize, FreeType freeType);

  [<UnmanagedFunctionPointer(CallingConvention.Cdecl)>] 
  type Int32ToInt32Delegate = delegate of int32 -> int32

open ExecutionEngineInternal

/// Allocate unmanaged executable memory and fill it with the given machine
/// code, exposing an Invoke member that executes the given code.
type ExecutionEngine(code: byte []) =
  let executableMemory =
    VirtualAlloc(IntPtr.Zero,
                 UIntPtr(uint32 code.Length),
                 AllocationType.COMMIT,
                 MemoryProtection.EXECUTE_READWRITE)
  let jitedFun =
    let ty = typeof<Int32ToInt32Delegate>
    Marshal.GetDelegateForFunctionPointer(executableMemory, ty)
      :?> Int32ToInt32Delegate
  let mutable isDisposed = false

  do
    Marshal.Copy(code, 0, executableMemory, code.Length)

  member __.Invoke (arg: int) : int =
    if isDisposed then
      failwith "Execution engine invoked after it was disposed"
    jitedFun.Invoke arg

  interface System.IDisposable with
    member __.Dispose() =
      isDisposed <- true
      VirtualFree(executableMemory, UIntPtr.Zero, FreeType.DECOMMIT)
      |> ignore
*)
