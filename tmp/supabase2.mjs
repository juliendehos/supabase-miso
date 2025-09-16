
import { createClient } from '@supabase/supabase-js'

const supabaseUrl = 'https://cmeicmtkrdbrelovyssz.supabase.co'
const supabaseAnonKey = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImNtZWljbXRrcmRicmVsb3Z5c3N6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTY0NTM2MTMsImV4cCI6MjA3MjAyOTYxM30._ga2HbuYt8JJTKYEQZc5ACAP2VT3KyjcbbV1Og0wEG0'
const supabase = createClient(supabaseUrl, supabaseAnonKey)

// listBuckets
// const resp = await supabase.storage.listBuckets()

// list
// const resp = await supabase.storage.from('avatars').list()
// const resp = await supabase.storage.from('avatars').list('')
// const resp = await supabase.storage.from('avatars').list('', {})
// const resp = await supabase.storage.from('avatars').list('test')
// const resp = await supabase.storage.from('avatars').list('test', {limit: 1, offset: 1})
// const resp = await supabase.storage.from('avatars').list('', {limit: 1, offset: 1})
const resp = await supabase.storage.from('avatars').list('', {limit: 10, search: "windsurf"})

console.log(resp)

