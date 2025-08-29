// dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
globalThis['runSupabase'] = function (namespace, fnName, args, successful, errorful) {
  globalThis['supabase'][namespace][fnName].apply(this, args).then(({ data, error }) => {
    if (data) successful(data);
    if (error) errorful(error);
  });
}
