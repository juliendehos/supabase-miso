import { createClient } from '@supabase/supabase-js'

console.log('howdy partner')

// Create a single supabase client for interacting with your database
globalThis ['createClient'] = createClient;

globalThis['supabase'] = createClient('https://bufjmcerlanfijatbwxu.supabase.co', 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJ1ZmptY2VybGFuZmlqYXRid3h1Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTUzMTgwNzgsImV4cCI6MjA3MDg5NDA3OH0.Cv8xhZyZTQuLVphhQ-fxbf6C4zTzu85I7leNyTMggKU')

console.log('Supabase Instance: ', globalThis['supabase'])
