
// dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
globalThis['runSupabase'] = function (namespace, fnName, args, successful, errorful) {
  const p = ({ data, error }) => {
      if (data) successful(data);
      if (error) errorful(error);
    };
  if (Array.isArray(args) && args.length>0) {
    globalThis['supabase'][namespace][fnName](args).then(p);
  } else {
    globalThis['supabase'][namespace][fnName]().then(p);
  }
}

globalThis['runSupabaseFrom'] = function (namespace, fromArg, fnName, args, successful, errorful) {
  const p = ({ data, error }) => {
      if (data) successful(data);
      if (error) errorful(error);
    };
  if (Array.isArray(args) && args.length>0) {
    globalThis['supabase'][namespace].from(fromArg)[fnName](args).then(p);
  } else {
    globalThis['supabase'][namespace].from(fromArg)[fnName]().then(p);
  }
}

