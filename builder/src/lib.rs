use proc_macro::TokenStream;
use proc_macro2::Span;
use quote;
use syn;
use syn::{DeriveInput, Ident, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);
    let identifier = ast.ident;
    let builder_identifier = Ident::new(&format!("{}Builder", identifier), Span::call_site());

    let fields = match ast.data {
        syn::Data::Struct(s) => match s.fields {
            syn::Fields::Named(named) => named
                .named
                .into_pairs()
                .map(|f| (f.value().ident.clone().unwrap(), f.value().clone().ty))
                .collect(),
            _ => {
                vec![]
            }
        },
        _ => vec![],
    };

    let field_idents: Vec<Ident> = fields.iter().map(|(i, _)| i.clone()).collect();
    let field_tys: Vec<Type> = fields.iter().map(|(_, t)| t.clone()).collect();

    let tokens = quote::quote!(
        use std::error::Error;
        impl #identifier {
            pub fn builder() -> #builder_identifier {
                #builder_identifier { #(#field_idents : None),* }
            }
        }

        pub struct #builder_identifier {
            #(#field_idents : Option<#field_tys>),*
        }

        impl #builder_identifier {
            #(
                pub fn #field_idents(&mut self, #field_idents: #field_tys) {
                    self.#field_idents = Some(#field_idents);
                }

            )*

            pub fn build(&mut self) -> Result<#identifier, Box<dyn Error>> {
                if [#(self.#field_idents.is_some()),*].iter().all(|x| *x == true) {
                    return
                        Ok(
                            #identifier {
                                #(#field_idents: self.#field_idents.clone().unwrap()),*
                            }
                        );
                }
                Err("xD".into())
            }

        }
    );

    eprintln!("TOKENS: \n{}", &tokens);
    tokens.into()
}
