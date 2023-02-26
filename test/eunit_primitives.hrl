-include_lib("stdlib/include/assert.hrl").

%% This composes a test set from the exported test functions of the named module, 
%%  i.e., those functions with arity zero whose names end with _test or _test_. 
%% Basically, the ..._test() functions become simple tests, while the ..._test_() 
%%  functions become generators.
%% In addition, EUnit will also look for another module whose name is ModuleName 
%%  plus the suffix _tests, and if it exists, all the tests from that module will 
%%  also be added. 
%% (If ModuleName already contains the suffix _tests, this is not done.) E.g., 
%%  the specification {module, mymodule} will run all tests in the modules mymodule 
%%  and mymodule_tests. Typically, the _tests module should only contain test cases 
%%  that use the public interface of the main module (and no other code).
-type ep_module() :: module() | {module, module()}.

%% =Arity 3=
%% This is a normal Erlang/OTP application descriptor, as found in an .app file. 
%% The resulting test set consists of the modules listed in the modules entry in Info.
%% =Arity 2=
%% This creates a test set from all the modules belonging to the specified application, 
%%  by consulting the application's .app file (see {file, FileName}), or if no such file exists, 
%%  by testing all object files in the application's ebin-directory (see {dir, Path}); 
%%  if that does not exist, the code:lib_dir(AppName) directory is used.
-type ep_application() :: {application, AppName::atom()}
                        | {application, AppName::atom(), Info::list()}.

%% A single string represents the path of a file or directory, and is equivalent to {file, Path}, 
%%  or {dir, Path}, respectively, depending on what Path refers to in the file system.
-type ep_path() :: Path::string().

%% If FileName has a suffix that indicates an object file (.beam), EUnit will try to reload the 
%%  module from the specified file and test it. 
%% Otherwise, the file is assumed to be a text file containing test specifications, which will 
%%  be read using the standard library function file:path_consult/2.
%% Unless the file name is absolute, the file is first searched for relative to the current 
%%  directory, and then using the normal search path (code:get_path()). 
%% This means that the names of typical "app" files can be used directly, without a path, 
%%  e.g., "mnesia.app".
-type ep_file() :: {file, FileName::string()}.

%% This tests all object files in the specified directory, as if they had been individually 
%%  specified using {file, FileName}.
-type ep_dir() :: {dir, Path::string()}.

%% =Arity 2=
%% The generator function GenFun is called to produce a test set.
%% =Arity 3=
%% The function ModuleName:FunctionName/0 is called to produce a test set.
-type ep_generator() :: {generator, fun(() -> list(eunit_primitive()))}
                      | {generator, module(), FunctionName::atom()}.

%% Distributes the value X over the unary functions in the list, turning them into nullary 
%%  test functions. 
%% An AbstractTestFun is like an ordinary test fun, but takes one argument instead of zero - 
%%  it's basically missing some information before it can be a proper test. 
%% In practice, `{with, X, [F_1, ..., F_N]}` is equivalent to 
%%  `[fun () -> F_1(X) end, ..., fun () -> F_N(X) end].` 
%% This is particularly useful if your abstract test functions are already implemented as proper 
%%  functions: `{with, FD, [fun filetest_a/1, fun filetest_b/1, fun filetest_c/1]}` is equivalent 
%%  to `[fun () -> filetest_a(FD) end, fun () -> filetest_b(FD) end, fun () -> filetest_c(FD) end]`, 
%%  but much more compact.
-type ep_with() :: {with, X :: any(), [fun((X :: any()) -> any())]}.

-type eunit_primitive() :: ep_module()
                         | ep_application()
                         | ep_path()
                         | ep_file()
                         | ep_dir()
                         | ep_generator()
                         | ep_with().

-define(EP_MODULE(Mod), begin 
    ?assert(is_atom(Mod)), 
    {module, Mod} 
end).

-define(EP_APPLICATION(AppName), begin 
    ?assert(is_atom(AppName)), 
    {application, AppName} 
end).
-define(EP_APPLICATION(AppName, Info), begin 
    ?assert(is_atom(AppName) andalso is_list(Info)), 
    {application, AppName, Info} 
end).

-define(EP_PATH(Path), begin 
    ?assert(is_list(Path)), 
    Path
end).

-define(EP_FILE(Path), begin 
    ?assert(is_list(Path)), 
    {file, Path}
end).

-define(EP_DIR(Path), begin 
    ?assert(is_list(Path)), 
    {dir, Path}
end).

-define(EP_GENERATOR(Fun), begin
    ?assert(is_function(Fun, 0)), 
    {generator, Fun}
end).

-define(EP_GENERATOR(Mod, Fun), begin
    ?assert(is_atom(Mod) andalso is_atom(Fun)),
    {generator, Mod, Fun}
end).

-define(EP_WITH(X, Funs), begin
    ?assert(is_list(Funs) andalso lists:all(fun(V) -> erlang:is_function(V, 1) end, Funs)),
    {with, X, Funs}
end).