
// dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
globalThis['runSupabase'] = function (namespace, fnName, args, successful, errorful) {

  console.log("runSupabase");
  console.log(namespace);
  console.log(fnName);
  console.log(args);

  globalThis['supabase'][namespace][fnName](...args)
    .then( ({ data, error }) => {
      if (data) successful(data);
      if (error) errorful(error);
    }
  )
}

globalThis['runSupabaseFrom'] = function (namespace, fromArg, fnName, args, successful, errorful) {

  console.log("runSupabaseFrom");
  console.log(namespace);
  console.log(fromArg);
  console.log(fnName);
  console.log(args);

  globalThis['supabase'][namespace].from(fromArg)[fnName](...args)
    .then( ({ data, error }) => {
      if (data) successful(data);
      if (error) errorful(error);
    }
  )
}


