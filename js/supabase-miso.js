// dmj: usage like: runSupabase('auth','signUp',successCallback, errorCallback);
globalThis['runSupabase'] = function (namespace, fnName, successful, errorful) {
  globalThis['supabase'][namespace][fnName].then(({ data, error }) => {
    if (data) successful(data);
    if (error) errorful(error);
  });
}
