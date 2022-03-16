use proc_macro::{Span, TokenStream};

use quote::{format_ident, quote, ToTokens};

use regex::Regex;

use syn::{
    parse_macro_input,
    punctuated::Punctuated,
    token::{Brace, Colon, Pub},
    Field, Fields, FieldsNamed, ItemStruct, Token, Type, VisPublic, Visibility,
};

#[proc_macro]
pub fn bit_field_accessors(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let re = Regex::new(r#"([a-zA-Z_])*\s*?(\d*)\s*?;"#).unwrap();
    let stringed = tokens.to_string();
    let mut outs = proc_macro2::TokenStream::new();
    outs.extend(re.captures_iter(&stringed).map(|item| {
        let items = item
            .get(0)
            .unwrap()
            .as_str()
            .split(' ')
            .collect::<Vec<&str>>();
        let ident = items[0];

        let bit = item[2].replace(r#"(\w*)?;?"#, "");
        let bit: u64 = bit.parse().unwrap();

        let get = format_ident!("get_{}", ident);
        let get_doc = format!("Get the value at the {bit}th bit\n");

        let set = format_ident!("set_{}", ident);
        let set_doc = format!("Set the value at the {bit}th bit\n");

        let clear = format_ident!("clear_{}", ident);
        let clear_doc = format!("Clear the value at the {bit}th bit\n");

        let toggle = format_ident!("toggle_{}", ident);
        let toggle_doc = format!("Toggle the value at the {bit}th bit");
        // println!("Get: {}\nSet: {}\nToggle: {}", get, set, toggle);

        let flag_val: u64 = 1 << bit;

        let flag = format_ident!("{}", ident.to_ascii_uppercase());
        let flag_doc = format!("Flag {flag} to build from OR with the value {flag_val}");

        quote! {
            #[doc = #flag_doc]
            pub const #flag: u64 = #flag_val;

            #[doc = #get_doc]
            pub const fn #get(&self) -> bool {
                (self.0 & Self::#flag) != 0
            }

            #[doc = #set_doc]
            pub const fn #set(&mut self) {
                self.0 |= Self::#flag;
            }

            #[doc = #clear_doc]
            pub const fn #clear(&mut self) {
                self.0 &= !Self::#flag;
            }

            #[doc = #toggle_doc]
            pub const fn #toggle(&mut self) {
                self.0 ^= Self::#flag;
            }
        }
    }));

    // println!("{outs}");

    outs.into()
}

/// Add the `identifier` and `next` fields to a given struct, using the provided number as its identifier
///
/// This will expect a struct called `BaseTag`, which contains only the `identifier` and `next` fields to exist,
/// as the `get_next` will return one.
///
/// This will panic if anything is in the wrong form, which is subject to change at any point with no prompting.
#[proc_macro_attribute]
pub fn form_tag(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ident = match attr.to_string().trim().is_empty() {
        false => u64::from_str_radix(attr.to_string().replace("0x", "").as_str(), 16)
            .expect("The `identifier` wasn't a valid hexidecimal number!"),
        true => 0,
    };
    let done = parse_macro_input!(item as ItemStruct);

    let mut base = Vec::new();

    base.push(Field {
        attrs: Vec::new(),
        vis: Visibility::Public(VisPublic {
            pub_token: Pub {
                span: Span::call_site().into(),
            },
        }),
        ident: Some(format_ident!("identifier")),
        colon_token: Some(Colon {
            spans: [Span::call_site().into(); 1],
        }),
        ty: Type::Verbatim("u64".parse::<TokenStream>().unwrap().into()),
    });

    base.push(Field {
        attrs: Vec::new(),
        vis: Visibility::Public(VisPublic {
            pub_token: Pub {
                span: Span::call_site().into(),
            },
        }),
        ident: Some(format_ident!("next")),
        colon_token: Some(Colon {
            spans: [Span::call_site().into(); 1],
        }),
        ty: Type::Verbatim("*const ()".parse::<TokenStream>().unwrap().into()),
    });

    base.extend(done.fields.iter().cloned());

    let mut field_punc: Punctuated<Field, Token![,]> = Punctuated::new();

    for i in base.iter() {
        field_punc.push(i.clone());
    }

    let new_struct = ItemStruct {
        attrs: done.attrs,
        vis: done.vis,
        struct_token: done.struct_token,
        ident: done.ident,
        generics: done.generics,
        fields: Fields::Named(FieldsNamed {
            brace_token: Brace([Span::call_site().into(); 1]),
            named: field_punc,
        }),
        semi_token: done.semi_token,
    };

    let mut new_final = proc_macro2::TokenStream::new();
    let new_ident = new_struct.ident.clone();

    let mut defaults = Vec::new();
    for i in base {
        if i.ident.clone().unwrap() == "identifier" {
            defaults.push(
                format!("{}: {}", i.ident.clone().unwrap(), ident)
                    .parse::<proc_macro2::TokenStream>()
                    .unwrap(),
            )
        } else if i.ident.clone().unwrap() == "next" {
            defaults.push(
                format!("{}: ::core::ptr::null()", i.ident.clone().unwrap())
                    .parse::<proc_macro2::TokenStream>()
                    .unwrap(),
            )
        } else {
            defaults.push(
                format!("{}: ::core::default::Default::default()", i.ident.unwrap())
                    .parse::<proc_macro2::TokenStream>()
                    .unwrap(),
            );
        }
    }

    new_struct.to_tokens(&mut new_final);

    quote! {
        #new_final

        impl #new_ident {
            pub const IDENTIFIER: u64 = #ident;

            pub fn new() -> Self {
                Self {
                    #( #defaults ),*
                }
            }

            /// Get the aligned size, in bytes, of the struct
            pub const fn get_size() -> usize {
                core::mem::size_of::<Self>()
            }

            /// Try to convert a base tag to a specific kind of tag
            pub fn try_from(value: *const BaseTag) -> Result<&'static Self, TagTryFromError> {
                if unsafe { (value as *const u64).read() } == Self::IDENTIFIER {
                    unsafe {
                        Ok( &*((value as *const BaseTag) as *const Self))
                    }
                } else {
                    Err(TagTryFromError::NoMatch)
                }
            }
        }

        unsafe impl Sync for #new_ident {}
    }
    .into()
}

/// Add the `identifier` and `next` fields to a given struct, using the provided number as its identifier.
/// This will not implement a `new` function. This also requires that there is a `length` field, for the unsized elements
///
/// This will expect a struct called `BaseTag`, which contains only the `identifier` and `next` fields to exist,
/// as the `get_next` will return one.
///
/// This will panic if anything is in the wrong form, which is subject to change at any point with no prompting.
#[proc_macro_attribute]
pub fn form_tag_unsized(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ident = match attr.to_string().trim().is_empty() {
        false => u64::from_str_radix(attr.to_string().replace("0x", "").as_str(), 16)
            .expect("The `identifier` wasn't a valid hexidecimal number!"),
        true => 0,
    };
    let done = parse_macro_input!(item as ItemStruct);

    let mut base = Vec::new();

    base.push(Field {
        attrs: Vec::new(),
        vis: Visibility::Public(VisPublic {
            pub_token: Pub {
                span: Span::call_site().into(),
            },
        }),
        ident: Some(format_ident!("identifier")),
        colon_token: Some(Colon {
            spans: [Span::call_site().into(); 1],
        }),
        ty: Type::Verbatim("u64".parse::<TokenStream>().unwrap().into()),
    });

    base.push(Field {
        attrs: Vec::new(),
        vis: Visibility::Public(VisPublic {
            pub_token: Pub {
                span: Span::call_site().into(),
            },
        }),
        ident: Some(format_ident!("next")),
        colon_token: Some(Colon {
            spans: [Span::call_site().into(); 1],
        }),
        ty: Type::Verbatim("*const ()".parse::<TokenStream>().unwrap().into()),
    });

    base.extend(done.fields.iter().cloned());

    let mut field_punc: Punctuated<Field, Token![,]> = Punctuated::new();

    for i in base.iter() {
        field_punc.push(i.clone());
    }

    let new_struct = ItemStruct {
        attrs: done.attrs,
        vis: done.vis,
        struct_token: done.struct_token,
        ident: done.ident,
        generics: done.generics,
        fields: Fields::Named(FieldsNamed {
            brace_token: Brace([Span::call_site().into(); 1]),
            named: field_punc,
        }),
        semi_token: done.semi_token,
    };

    let mut new_final = proc_macro2::TokenStream::new();
    let new_ident = new_struct.ident.clone();
    let new_ident_string = new_ident.to_string();

    new_struct.to_tokens(&mut new_final);

    let mut debug = Vec::new();
    debug.push(format!("f.debug_struct(\"{}\")", new_ident_string));
    for i in new_struct.fields {
        let ident = i.ident.clone().unwrap();
        debug.push(format!(".field(\"{}\", &self.{})", ident, ident));
    }
    let _ = debug.pop();
    debug.push(".finish_non_exhaustive()".to_string());
    let debug = debug.iter().map(|i| i.as_str()).collect::<String>();
    let debug_impl = debug.parse::<proc_macro2::TokenStream>().unwrap(); 

    quote! {
        #new_final

        impl #new_ident {
            pub const IDENTIFIER: u64 = #ident;

            /// Try to convert a base tag to a specific kind of tag
            pub fn try_from_base(value: *const BaseTag) -> Result<*const Self, TagTryFromError> {
                if unsafe {  (value as *const u64).read() } == Self::IDENTIFIER {
                    unsafe {
                        let len = (value as *const u64).offset(2).read_volatile();
                        let ptr = core::ptr::slice_from_raw_parts(value as *const (), len.try_into().unwrap());
                        Ok(ptr as *const Self)
                    }
                } else {
                    Err(TagTryFromError::NoMatch)
                }
            }
        }
        
        unsafe impl Sync for #new_ident {}

        impl ::core::fmt::Debug for #new_ident {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                #debug_impl
            }
        }
    }
    .into()
}
